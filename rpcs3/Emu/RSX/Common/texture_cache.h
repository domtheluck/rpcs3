#pragma once

#include "../rsx_cache.h"
#include "../rsx_utils.h"

#include <atomic>

namespace rsx
{
	template <typename section_storage_type, typename image_view_type, typename image_storage_type, typename texture_format>
	class texture_cache
	{
	protected:

		struct ranged_storage
		{
			std::vector<section_storage_type> data;  //Stored data
			std::atomic_int valid_count = { 0 };  //Number of usable (non-dirty) blocks
			u32 max_range = 0;  //Largest stored block

			void notify(u32 data_size)
			{
				max_range = std::max(data_size, max_range);
				valid_count++;
			}

			void add(section_storage_type& section, u32 data_size)
			{
				max_range = std::max(data_size, max_range);
				valid_count++;

				data.push_back(std::move(section));
			}
		};

		// Keep track of cache misses to pre-emptively flush some addresses
		struct framebuffer_memory_characteristics
		{
			u32 misses;
			u32 block_size;
			texture_format format;
		};

		std::atomic_bool in_access_violation_handler = { false };
		shared_mutex m_cache_mutex;
		std::unordered_map<u32, ranged_storage> m_cache;

		std::pair<u32, u32> read_only_range = std::make_pair(0xFFFFFFFF, 0);
		std::pair<u32, u32> no_access_range = std::make_pair(0xFFFFFFFF, 0);

		std::unordered_map<u32, framebuffer_memory_characteristics> m_cache_miss_statistics_table;
		
		//Memory usage
		const s32 m_max_zombie_objects = 32; //Limit on how many texture objects to keep around for reuse after they are invalidated
		s32 m_unreleased_texture_objects = 0; //Number of invalidated objects not yet freed from memory
		
		virtual void free_texture_section(section_storage_type&) = 0;

	public:

		texture_cache() {}
		~texture_cache() {}
		
		virtual void destroy() = 0;
		virtual bool is_depth_texture(const u32) = 0;
		virtual void on_frame_end() = 0;

		section_storage_type *find_texture_from_range(u32 rsx_address, u32 range)
		{
			auto test = std::make_pair(rsx_address, range);
			for (auto &address_range : m_cache)
			{
				auto &range_data = address_range.second;
				for (auto &tex : range_data.data)
				{
					if (tex.get_section_base() > rsx_address)
						continue;

					if (!tex.is_dirty() && tex.overlaps(test, true))
						return &tex;
				}
			}

			return nullptr;
		}

		section_storage_type *find_texture_from_dimensions(u32 rsx_address, u16 width = 0, u16 height = 0, u16 mipmaps = 0)
		{
			auto found = m_cache.find(rsx_address);
			if (found != m_cache.end())
			{
				auto &range_data = found->second;
				for (auto &tex : range_data.data)
				{
					if (tex.matches(rsx_address, width, height, mipmaps) && !tex.is_dirty())
					{
						return &tex;
					}
				}
			}

			return nullptr;
		}

		section_storage_type& find_cached_texture(u32 rsx_address, u32 rsx_size, bool confirm_dimensions = false, u16 width = 0, u16 height = 0, u16 mipmaps = 0)
		{
			{
				reader_lock lock(m_cache_mutex);

				auto found = m_cache.find(rsx_address);
				if (found != m_cache.end())
				{
					auto &range_data = found->second;

					for (auto &tex : range_data.data)
					{
						if (tex.matches(rsx_address, rsx_size) && !tex.is_dirty())
						{
							if (!confirm_dimensions) return tex;

							if (tex.matches(rsx_address, width, height, mipmaps))
								return tex;
							else
							{
								LOG_ERROR(RSX, "Cached object for address 0x%X was found, but it does not match stored parameters.", rsx_address);
								LOG_ERROR(RSX, "%d x %d vs %d x %d", width, height, tex.get_width(), tex.get_height());
							}
						}
					}

					for (auto &tex : range_data.data)
					{
						if (tex.is_dirty())
						{
							if (tex.exists())
							{
								m_unreleased_texture_objects--;
								free_texture_section(tex);
							}

							range_data.notify(rsx_size);
							return tex;
						}
					}
				}
			}

			writer_lock lock(m_cache_mutex);

			section_storage_type tmp;
			m_cache[rsx_address].add(tmp, rsx_size);
			return m_cache[rsx_address].data.back();
		}

		section_storage_type* find_flushable_section(const u32 address, const u32 range)
		{
			reader_lock lock(m_cache_mutex);

			auto found = m_cache.find(address);
			if (found != m_cache.end())
			{
				auto &range_data = found->second;
				for (auto &tex : range_data.data)
				{
					if (tex.is_dirty()) continue;
					if (!tex.is_flushable() && !tex.is_flushed()) continue;

					if (tex.matches(address, range))
						return &tex;
				}
			}

			return nullptr;
		}

		template <typename ...Args>
		void lock_memory_region(image_storage_type* image, const u32 memory_address, const u32 memory_size, const u32 width, const u32 height, const u32 pitch, Args&&... extras)
		{
			section_storage_type& region = find_cached_texture(memory_address, memory_size, true, width, height, 1);

			writer_lock lock(m_cache_mutex);

			if (!region.is_locked())
			{
				region.reset(memory_address, memory_size);
				region.set_dirty(false);
				no_access_range = region.get_min_max(no_access_range);
			}

			region.protect(utils::protection::no);
			region.create(width, height, 1, 1, nullptr, image, pitch, false, std::forward<Args>(extras)...);
		}

		template <typename ...Args>
		bool flush_memory_to_cache(const u32 memory_address, const u32 memory_size, bool skip_synchronized, Args&&... extra)
		{
			section_storage_type* region = find_flushable_section(memory_address, memory_size);

			//TODO: Make this an assertion
			if (region == nullptr)
			{
				LOG_ERROR(RSX, "Failed to find section for render target 0x%X + 0x%X", memory_address, memory_size);
				return false;
			}

			if (skip_synchronized && region->is_synchronized())
				return false;

			region->copy_texture(false, std::forward<Args>(extra)...);
			return true;
		}
		
		template <typename ...Args>
		bool load_memory_from_cache(const u32 memory_address, const u32 memory_size, Args&&... extras)
		{
			section_storage_type *region = find_flushable_section(memory_address, memory_size);

			if (region && !region->is_dirty())
			{
				region->fill_texture(std::forward<Args>(extras)...);
				return true;
			}

			//No valid object found in cache
			return false;
		}

		std::tuple<bool, section_storage_type*> address_is_flushable(u32 address)
		{
			if (address < no_access_range.first ||
				address > no_access_range.second)
				return std::make_tuple(false, nullptr);

			reader_lock lock(m_cache_mutex);

			auto found = m_cache.find(address);
			if (found != m_cache.end())
			{
				auto &range_data = found->second;
				for (auto &tex : range_data.data)
				{
					if (tex.is_dirty()) continue;
					if (!tex.is_flushable()) continue;

					if (tex.overlaps(address))
						return std::make_tuple(true, &tex);
				}
			}

			for (auto &address_range : m_cache)
			{
				if (address_range.first == address)
					continue;

				auto &range_data = address_range.second;

				//Quickly discard range
				const u32 lock_base = address_range.first & ~0xfff;
				const u32 lock_limit = align(range_data.max_range + address_range.first, 4096);

				if (address < lock_base || address >= lock_limit)
					continue;

				for (auto &tex : range_data.data)
				{
					if (tex.is_dirty()) continue;
					if (!tex.is_flushable()) continue;

					if (tex.overlaps(address))
						return std::make_tuple(true, &tex);
				}
			}

			return std::make_tuple(false, nullptr);
		}

		template <typename ...Args>
		bool flush_address(u32 address, Args&&... extras)
		{
			if (address < no_access_range.first ||
				address > no_access_range.second)
				return false;

			bool response = false;
			std::pair<u32, u32> trampled_range = std::make_pair(0xffffffff, 0x0);
			std::unordered_map<u32, bool> processed_ranges;

			rsx::conditional_lock<shared_mutex> lock(in_access_violation_handler, m_cache_mutex);

			for (auto It = m_cache.begin(); It != m_cache.end(); It++)
			{
				auto &range_data = It->second;
				const u32 base = It->first;
				bool range_reset = false;

				if (processed_ranges[base] || range_data.valid_count == 0)
					continue;

				//Quickly discard range
				const u32 lock_base = base & ~0xfff;
				const u32 lock_limit = align(range_data.max_range + base, 4096);

				if ((trampled_range.first >= lock_limit || lock_base >= trampled_range.second) &&
					(lock_base > address || lock_limit <= address))
				{
					processed_ranges[base] = true;
					continue;
				}

				for (int i = 0; i < range_data.data.size(); i++)
				{
					auto &tex = range_data.data[i];

					if (tex.is_dirty()) continue;
					if (!tex.is_flushable()) continue;

					auto overlapped = tex.overlaps_page(trampled_range, address);
					if (std::get<0>(overlapped))
					{
						auto &new_range = std::get<1>(overlapped);

						if (new_range.first != trampled_range.first ||
							new_range.second != trampled_range.second)
						{
							i = 0;
							trampled_range = new_range;
							range_reset = true;
						}

						//TODO: Map basic host_visible memory without coherent constraint
						if (!tex.flush(std::forward<Args>(extras)...))
						{
							//Missed address, note this
							//TODO: Lower severity when successful to keep the cache from overworking
							record_cache_miss(tex);
						}

						response = true;
					}
				}

				if (range_reset)
				{
					processed_ranges.clear();
					It = m_cache.begin();
				}

				processed_ranges[base] = true;
			}

			return response;
		}

		bool invalidate_address(u32 address)
		{
			return invalidate_range(address, 4096 - (address & 4095));
		}

		bool invalidate_range(u32 address, u32 range, bool unprotect = true)
		{
			std::pair<u32, u32> trampled_range = std::make_pair(address, address + range);

			if (trampled_range.second < read_only_range.first ||
				trampled_range.first > read_only_range.second)
			{
				//Doesnt fall in the read_only textures range; check render targets
				if (trampled_range.second < no_access_range.first ||
					trampled_range.first > no_access_range.second)
					return false;
			}

			bool response = false;
			std::unordered_map<u32, bool> processed_ranges;

			rsx::conditional_lock<shared_mutex> lock(in_access_violation_handler, m_cache_mutex);

			for (auto It = m_cache.begin(); It != m_cache.end(); It++)
			{
				auto &range_data = It->second;
				const u32 base = It->first;
				bool range_reset = false;

				if (processed_ranges[base] || range_data.valid_count == 0)
					continue;

				//Quickly discard range
				const u32 lock_base = base & ~0xfff;
				const u32 lock_limit = align(range_data.max_range + base, 4096);

				if (trampled_range.first >= lock_limit || lock_base >= trampled_range.second)
				{
					processed_ranges[base] = true;
					continue;
				}

				for (int i = 0; i < range_data.data.size(); i++)
				{
					auto &tex = range_data.data[i];

					if (tex.is_dirty()) continue;
					if (!tex.is_locked()) continue;	//flushable sections can be 'clean' but unlocked. TODO: Handle this better

					auto overlapped = tex.overlaps_page(trampled_range, address);
					if (std::get<0>(overlapped))
					{
						auto &new_range = std::get<1>(overlapped);

						if (new_range.first != trampled_range.first ||
							new_range.second != trampled_range.second)
						{
							i = 0;
							trampled_range = new_range;
							range_reset = true;
						}

						if (unprotect)
						{
							m_unreleased_texture_objects++;

							tex.set_dirty(true);
							tex.unprotect();
						}
						else
						{
							tex.discard();
						}

						range_data.valid_count--;
						response = true;
					}
				}

				if (range_reset)
				{
					processed_ranges.clear();
					It = m_cache.begin();
				}

				processed_ranges[base] = true;
			}

			return response;
		}

		void record_cache_miss(section_storage_type &tex)
		{
			const u32 memory_address = tex.get_section_base();
			const u32 memory_size = tex.get_section_size();
			const auto fmt = tex.get_format();

			auto It = m_cache_miss_statistics_table.find(memory_address);
			if (It == m_cache_miss_statistics_table.end())
			{
				m_cache_miss_statistics_table[memory_address] = { 1, memory_size, fmt };
				return;
			}

			auto &value = It->second;
			if (value.format != fmt || value.block_size != memory_size)
			{
				m_cache_miss_statistics_table[memory_address] = { 1, memory_size, fmt };
				return;
			}

			value.misses++;
		}

		template <typename ...Args>
		void flush_if_cache_miss_likely(const texture_format fmt, const u32 memory_address, const u32 memory_size, Args&&... extras)
		{
			auto It = m_cache_miss_statistics_table.find(memory_address);
			if (It == m_cache_miss_statistics_table.end())
			{
				m_cache_miss_statistics_table[memory_address] = { 0, memory_size, fmt };
				return;
			}

			auto &value = It->second;

			if (value.format != fmt || value.block_size != memory_size)
			{
				//Reset since the data has changed
				//TODO: Keep track of all this information together
				m_cache_miss_statistics_table[memory_address] = { 0, memory_size, fmt };
				return;
			}

			//Properly synchronized - no miss
			if (!value.misses) return;

			//Auto flush if this address keeps missing (not properly synchronized)
			if (value.misses > 16)
			{
				//TODO: Determine better way of setting threshold
				if (!flush_memory_to_cache(memory_address, memory_size, true, std::forward<Args>(extras)...))
					value.misses--;
			}
		}
		
		void purge_dirty()
		{
			//Reclaims all graphics memory consumed by dirty textures
			std::vector<u32> empty_addresses;
			empty_addresses.resize(32);

			for (auto &address_range : m_cache)
			{
				auto &range_data = address_range.second;

				if (range_data.valid_count == 0)
					empty_addresses.push_back(address_range.first);

				for (auto &tex : range_data.data)
				{
					if (!tex.is_dirty())
						continue;

					free_texture_section(tex);
				}
			}

			//Free descriptor objects as well
			for (const auto &address : empty_addresses)
			{
				m_cache.erase(address);
			}
			
			m_unreleased_texture_objects = 0;
		}
	};
}
