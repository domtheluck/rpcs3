#pragma once

#include "stdafx.h"

#include <exception>
#include <string>
#include <functional>
#include <vector>
#include <memory>
#include <thread>
#include <condition_variable>
#include <chrono>

#include "Utilities/mutex.h"
#include "Emu/System.h"
#include "GLRenderTargets.h"
#include "../Common/TextureUtils.h"
#include "../Common/texture_cache.h"
#include "../../Memory/vm.h"
#include "../rsx_utils.h"

class GLGSRender;

namespace gl
{
	class cached_texture_section : public rsx::buffered_section
	{
	private:
		fence m_fence;
		u32 pbo_id = 0;
		u32 pbo_size = 0;

		u32 vram_texture = 0;

		bool copied = false;
		bool flushed = false;
		bool is_depth = false;

		u32 current_width = 0;
		u32 current_height = 0;
		u32 current_pitch = 0;
		u32 real_pitch = 0;

		texture::format format = texture::format::rgba;
		texture::type type = texture::type::ubyte;
		bool pack_unpack_swap_bytes = false;

		u8 get_pixel_size(texture::format fmt_, texture::type type_)
		{
			u8 size = 1;
			switch (type_)
			{
			case texture::type::ubyte:
			case texture::type::sbyte:
				break;
			case texture::type::ushort:
			case texture::type::sshort:
			case texture::type::f16:
				size = 2;
				break;
			case texture::type::ushort_5_6_5:
			case texture::type::ushort_5_6_5_rev:
			case texture::type::ushort_4_4_4_4:
			case texture::type::ushort_4_4_4_4_rev:
			case texture::type::ushort_5_5_5_1:
			case texture::type::ushort_1_5_5_5_rev:
				return 2;
			case texture::type::uint_8_8_8_8:
			case texture::type::uint_8_8_8_8_rev:
			case texture::type::uint_10_10_10_2:
			case texture::type::uint_2_10_10_10_rev:
			case texture::type::uint_24_8:
				return 4;
			case texture::type::f32:
			case texture::type::sint:
			case texture::type::uint:
				size = 4;
				break;
			}

			switch (fmt_)
			{
			case texture::format::red:
			case texture::format::r:
				break;
			case texture::format::rg:
				size *= 2;
				break;
			case texture::format::rgb:
			case texture::format::bgr:
				size *= 3;
				break;
			case texture::format::rgba:
			case texture::format::bgra:
				size *= 4;
				break;

			//Depth formats..
			case texture::format::depth:
				size = 2;
				break;
			case texture::format::depth_stencil:
				size = 4;
				break;
			default:
				LOG_ERROR(RSX, "Unsupported rtt format %d", (GLenum)fmt_);
				size = 4;
			}

			return size;
		}

		void init_buffer()
		{
			if (pbo_id)
			{
				glDeleteBuffers(1, &pbo_id);
				pbo_id = 0;
				pbo_size = 0;
			}

			glGenBuffers(1, &pbo_id);

			const u32 buffer_size = align(cpu_address_range, 4096);
			glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo_id);
			glBufferStorage(GL_PIXEL_PACK_BUFFER, buffer_size, nullptr, GL_MAP_READ_BIT);

			pbo_size = buffer_size;
		}

	public:

		void reset(const u32 base, const u32 size, const bool flushable=false)
		{
			rsx::protection_policy policy = g_cfg.video.strict_rendering_mode ? rsx::protection_policy::protect_policy_full_range : rsx::protection_policy::protect_policy_one_page;
			rsx::buffered_section::reset(base, size, policy);

			if (flushable)
				init_buffer();
			
			flushed = false;
			copied = false;
			is_depth = false;

			vram_texture = 0;
		}
		
		void create(const u16 w, const u16 h, const u16 /*depth*/, const u16 /*mipmaps*/, void*,
				gl::texture* image, const u32 native_pitch, bool read_only,
				gl::texture::format gl_format, gl::texture::type gl_type, bool swap_bytes)
		{
			if (!read_only && pbo_id == 0)
				init_buffer();
				
			current_width = w;
			current_height = h;
			current_pitch = native_pitch;

			vram_texture = image->id();
			set_format(gl_format, gl_type, swap_bytes);
		}

		void create_read_only(const u32 id, const u32 width, const u32 height)
		{
			//Only to be used for ro memory, we dont care about most members, just dimensions and the vram texture handle
			current_width = width;
			current_height = height;
			vram_texture = id;

			current_pitch = 0;
			real_pitch = 0;
		}

		bool matches(const u32 rsx_address, const u32 rsx_size)
		{
			return rsx::buffered_section::matches(rsx_address, rsx_size);
		}

		bool matches(const u32 rsx_address, const u32 width, const u32 height, const u32 mipmaps)
		{
			if (cpu_address_base == rsx_address && !dirty)
			{
				//Mostly only used to debug; matches textures without checking dimensions
				if (width == 0 && height == 0)
					return true;

				return (current_width == width && current_height == height);
			}

			return false;
		}

		void set_dimensions(u32 width, u32 height, u32 pitch)
		{
			current_width = width;
			current_height = height;
			current_pitch = pitch;

			real_pitch = width * get_pixel_size(format, type);
		}

		void set_format(const texture::format gl_format, const texture::type gl_type, const bool swap_bytes)
		{
			format = gl_format;
			type = gl_type;
			pack_unpack_swap_bytes = swap_bytes;

			real_pitch = current_width * get_pixel_size(format, type);
		}

		void set_depth_flag(bool is_depth_fmt)
		{
			is_depth = is_depth_fmt;
		}

		void set_source(gl::texture &source)
		{
			vram_texture = source.id();
		}

		void copy_texture(bool=false)
		{
			if (!glIsTexture(vram_texture))
			{
				LOG_ERROR(RSX, "Attempted to download rtt texture, but texture handle was invalid! (0x%X)", vram_texture);
				return;
			}

			glPixelStorei(GL_PACK_SWAP_BYTES, pack_unpack_swap_bytes);
			glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo_id);

			if (get_driver_caps().EXT_dsa_supported)
				glGetTextureImageEXT(vram_texture, GL_TEXTURE_2D, 0, (GLenum)format, (GLenum)type, nullptr);
			else
				glGetTextureImage(vram_texture, 0, (GLenum)format, (GLenum)type, pbo_size, nullptr);

			glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);

			m_fence.reset();
			copied = true;
		}

		void fill_texture(gl::texture* tex)
		{
			if (!copied)
			{
				//LOG_WARNING(RSX, "Request to fill texture rejected because contents were not read");
				return;
			}

			u32 min_width = std::min((u32)tex->width(), current_width);
			u32 min_height = std::min((u32)tex->height(), current_height);

			tex->bind();
			glPixelStorei(GL_UNPACK_SWAP_BYTES, pack_unpack_swap_bytes);
			glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pbo_id);
			glTexSubImage2D((GLenum)tex->get_target(), 0, 0, 0, min_width, min_height, (GLenum)format, (GLenum)type, nullptr);
			glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
		}

		bool flush()
		{
			if (!copied)
			{
				LOG_WARNING(RSX, "Cache miss at address 0x%X. This is gonna hurt...", cpu_address_base);
				copy_texture();

				if (!copied)
				{
					LOG_WARNING(RSX, "Nothing to copy; Setting section to readable and moving on...");
					protect(utils::protection::ro);
					return false;
				}
			}

			protect(utils::protection::rw);
			m_fence.wait_for_signal();
			flushed = true;

			glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo_id);
			void *data = glMapBufferRange(GL_PIXEL_PACK_BUFFER, 0, pbo_size, GL_MAP_READ_BIT);
			u8 *dst = vm::ps3::_ptr<u8>(cpu_address_base);

			//throw if map failed since we'll segfault anyway
			verify(HERE), data != nullptr;

			if (real_pitch >= current_pitch)
			{
				memcpy(dst, data, cpu_address_range);
			}
			else
			{
				//TODO: Use compression hint from the gcm tile information
				//TODO: Fall back to bilinear filtering if samples > 2

				const u8 pixel_size = get_pixel_size(format, type);
				const u8 samples = current_pitch / real_pitch;
				rsx::scale_image_nearest(dst, const_cast<const void*>(data), current_width, current_height, current_pitch, real_pitch, pixel_size, samples);
			}

			glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
			glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
			protect(utils::protection::ro);
			
			return true;
		}

		void destroy()
		{
			if (!locked && pbo_id == 0 && vram_texture == 0 && m_fence.is_empty())
				//Already destroyed
				return;

			if (locked)
				unprotect();

			if (pbo_id == 0)
			{
				//Read-only texture, destroy texture memory
				glDeleteTextures(1, &vram_texture);
				vram_texture = 0;
			}
			else
			{
				//Destroy pbo cache since vram texture is managed elsewhere
				glDeleteBuffers(1, &pbo_id);
				pbo_id = 0;
				pbo_size = 0;
			}

			m_fence.destroy();
		}
		
		texture::format get_format() const
		{
			return format;
		}
		
		bool exists() const
		{
			return vram_texture != 0;
		}
		
		bool is_flushable() const
		{
			return pbo_id != 0;
		}

		bool is_flushed() const
		{
			return flushed;
		}

		bool is_synchronized() const
		{
			return copied;
		}

		void set_flushed(const bool state)
		{
			flushed = state;
		}

		bool is_empty() const
		{
			return vram_texture == 0;
		}

		const u32 id()
		{
			return vram_texture;
		}

		u32 get_width() const
		{
			return current_width;
		}
		
		u32 get_height() const
		{
			return current_height;
		}

		bool is_depth_texture() const
		{
			return is_depth;
		}
	};
		
	class texture_cache : public rsx::texture_cache<cached_texture_section, void*, gl::texture, gl::texture::format>
	{
	private:
	
		class blitter
		{
			fbo blit_src;
			fbo blit_dst;

		public:

			void init()
			{
				blit_src.create();
				blit_dst.create();
			}

			void destroy()
			{
				blit_dst.remove();
				blit_src.remove();
			}

			u32 scale_image(u32 src, u32 dst, const areai src_rect, const areai dst_rect, const GLenum dst_format, const position2i dst_offset, const position2i /*clip_offset*/,
					const size2i dst_dims, const size2i clip_dims, bool /*is_argb8*/, bool is_depth_copy, bool linear_interpolation)
			{
				s32 old_fbo = 0;
				glGetIntegerv(GL_FRAMEBUFFER_BINDING, &old_fbo);
				
				u32 dst_tex = dst;
				filter interp = linear_interpolation ? filter::linear : filter::nearest;

				if (!dst_tex)
				{
					glGenTextures(1, &dst_tex);
					glBindTexture(GL_TEXTURE_2D, dst_tex);
					glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
					glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
					glTexStorage2D(GL_TEXTURE_2D, 1, dst_format, dst_dims.width, dst_dims.height);
				}

				GLenum attachment = is_depth_copy ? GL_DEPTH_ATTACHMENT : GL_COLOR_ATTACHMENT0;

				blit_src.bind();
				glFramebufferTexture2D(GL_FRAMEBUFFER, attachment, GL_TEXTURE_2D, src, 0);
				blit_src.check();

				blit_dst.bind();
				glFramebufferTexture2D(GL_FRAMEBUFFER, attachment, GL_TEXTURE_2D, dst_tex, 0);
				blit_dst.check();

				u32 src_width = src_rect.x2 - src_rect.x1;
				u32 src_height = src_rect.y2 - src_rect.y1;
				u32 dst_width = dst_rect.x2 - dst_rect.x1;
				u32 dst_height = dst_rect.y2 - dst_rect.y1;

				if (clip_dims.width != dst_width ||
					clip_dims.height != dst_height)
				{
					//clip reproject
					src_width = (src_width * clip_dims.width) / dst_width;
					src_height = (src_height * clip_dims.height) / dst_height;
				}

				GLboolean scissor_test_enabled = glIsEnabled(GL_SCISSOR_TEST);
				if (scissor_test_enabled)
					glDisable(GL_SCISSOR_TEST);

				areai dst_area = dst_rect;
				dst_area.x1 += dst_offset.x;
				dst_area.x2 += dst_offset.x;
				dst_area.y1 += dst_offset.y;
				dst_area.y2 += dst_offset.y;

				blit_src.blit(blit_dst, src_rect, dst_area, is_depth_copy ? buffers::depth : buffers::color, interp);

				if (scissor_test_enabled)
					glEnable(GL_SCISSOR_TEST);

				glBindFramebuffer(GL_FRAMEBUFFER, old_fbo);
				return dst_tex;
			}
		};

	private:

		blitter m_hw_blitter;
		std::vector<u32> m_temporary_surfaces;

		cached_texture_section& create_texture(u32 id, u32 texaddr, u32 texsize, u32 w, u32 h)
		{
			cached_texture_section& tex = find_cached_texture(texaddr, texsize, true, w, h);
			tex.reset(texaddr, texsize, false);
			tex.create_read_only(id, w, h);
			read_only_range = tex.get_min_max(read_only_range);
			return tex;
		}

		void clear()
		{
			for (auto &address_range : m_cache)
			{
				auto &range_data = address_range.second;
				for (auto &tex : range_data.data)
				{
					tex.destroy();
				}

				range_data.data.resize(0);
			}

			clear_temporary_subresources();
			m_unreleased_texture_objects = 0;
		}
		
		void clear_temporary_subresources()
		{
			for (u32 &id : m_temporary_surfaces)
			{
				glDeleteTextures(1, &id);
			}

			m_temporary_surfaces.resize(0);
		}

		u32 create_temporary_subresource(u32 src_id, GLenum sized_internal_fmt, u16 x, u16 y, u16 width, u16 height)
		{
			u32 dst_id = 0;

			glGenTextures(1, &dst_id);
			glBindTexture(GL_TEXTURE_2D, dst_id);

			glTexStorage2D(GL_TEXTURE_2D, 1, sized_internal_fmt, width, height);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

			//Empty GL_ERROR
			glGetError();

			glCopyImageSubData(src_id, GL_TEXTURE_2D, 0, x, y, 0,
				dst_id, GL_TEXTURE_2D, 0, 0, 0, 0, width, height, 1);

			m_temporary_surfaces.push_back(dst_id);

			//Check for error
			if (GLenum err = glGetError())
			{
				LOG_WARNING(RSX, "Failed to copy image subresource with GL error 0x%X", err);
				return 0;
			}

			return dst_id;
		}
		
	protected:
		
		void free_texture_section(cached_texture_section& tex) override
		{
			tex.destroy();
		}

	public:

		texture_cache() {}

		~texture_cache() {}

		void initialize()
		{
			m_hw_blitter.init();
		}

		void destroy() override
		{
			clear();
			m_hw_blitter.destroy();
		}
		
		bool is_depth_texture(const u32 rsx_address) override
		{
			auto section = find_texture_from_range(rsx_address, 64u);
			if (section != nullptr) return section->is_depth_texture();

			return false;
		}

		void on_frame_end() override
		{
			if (m_unreleased_texture_objects >= m_max_zombie_objects)
			{
				purge_dirty();
			}
			
			clear_temporary_subresources();
		}

		template<typename RsxTextureType>
		void upload_texture(int index, RsxTextureType &tex, rsx::gl::texture &gl_texture, gl_render_targets &m_rtts)
		{
			const u32 texaddr = rsx::get_address(tex.offset(), tex.location());
			const u32 range = (u32)get_texture_size(tex);
			
			const u32 format = tex.format() & ~(CELL_GCM_TEXTURE_LN | CELL_GCM_TEXTURE_UN);
			const u32 tex_width = tex.width();
			const u32 tex_height = tex.height();
			const u32 native_pitch = (tex_width * get_format_block_size_in_bytes(format));
			const u32 tex_pitch = (tex.pitch() == 0)? native_pitch: tex.pitch();

			if (!texaddr || !range)
			{
				LOG_ERROR(RSX, "Texture upload requested but texture not found, (address=0x%X, size=0x%X)", texaddr, range);
				gl_texture.bind();
				return;
			}

			glActiveTexture(GL_TEXTURE0 + index);

			/**
			 * Check for sampleable rtts from previous render passes
			 */
			gl::render_target *texptr = nullptr;
			if ((texptr = m_rtts.get_texture_from_render_target_if_applicable(texaddr)))
			{
				for (const auto& tex : m_rtts.m_bound_render_targets)
				{
					if (std::get<0>(tex) == texaddr)
					{
						if (g_cfg.video.strict_rendering_mode)
						{
							LOG_WARNING(RSX, "Attempting to sample a currently bound render target @ 0x%x", texaddr);
							create_temporary_subresource(texptr->id(), (GLenum)texptr->get_compatible_internal_format(), 0, 0, texptr->width(), texptr->height());
							return;
						}
						else
						{
							//issue a texture barrier to ensure previous writes are visible
							auto &caps = gl::get_driver_caps();

							if (caps.ARB_texture_barrier_supported)
								glTextureBarrier();
							else if (caps.NV_texture_barrier_supported)
								glTextureBarrierNV();

							break;
						}
					}
				}

				texptr->bind();
				return;
			}

			if ((texptr = m_rtts.get_texture_from_depth_stencil_if_applicable(texaddr)))
			{
				if (texaddr == std::get<0>(m_rtts.m_bound_depth_stencil))
				{
					if (g_cfg.video.strict_rendering_mode)
					{
						LOG_WARNING(RSX, "Attempting to sample a currently bound depth surface @ 0x%x", texaddr);
						create_temporary_subresource(texptr->id(), (GLenum)texptr->get_compatible_internal_format(), 0, 0, texptr->width(), texptr->height());
						return;
					}
					else
					{
						//issue a texture barrier to ensure previous writes are visible
						auto &caps = gl::get_driver_caps();

						if (caps.ARB_texture_barrier_supported)
							glTextureBarrier();
						else if (caps.NV_texture_barrier_supported)
							glTextureBarrierNV();
					}
				}

				texptr->bind();
				return;
			}

			/**
			 * Check if we are re-sampling a subresource of an RTV/DSV texture, bound or otherwise
			 * (Turbo: Super Stunt Squad does this; bypassing the need for a sync object)
			 * The engine does not read back the texture resource through cell, but specifies a texture location that is
			 * a bound render target. We can bypass the expensive download in this case
			 */

			const f32 internal_scale = (f32)tex_pitch / native_pitch;
			const u32 internal_width = (const u32)(tex_width * internal_scale);

			const auto rsc = m_rtts.get_surface_subresource_if_applicable(texaddr, internal_width, tex_height, tex_pitch, true);
			if (rsc.surface)
			{
				//TODO: Check that this region is not cpu-dirty before doing a copy

				if (tex.get_extended_texture_dimension() != rsx::texture_dimension_extended::texture_dimension_2d)
				{
					LOG_ERROR(RSX, "Sampling of RTT region as non-2D texture! addr=0x%x, Type=%d, dims=%dx%d",
							texaddr, (u8)tex.get_extended_texture_dimension(), tex.width(), tex.height());
				}
				else
				{
					u32 bound_index = ~0U;

					bool dst_is_compressed = (format == CELL_GCM_TEXTURE_COMPRESSED_DXT1 || format == CELL_GCM_TEXTURE_COMPRESSED_DXT23 || format == CELL_GCM_TEXTURE_COMPRESSED_DXT45);

					if (!dst_is_compressed)
					{
						GLenum src_format = (GLenum)rsc.surface->get_internal_format();
						GLenum dst_format = std::get<0>(get_format_type(format));

						if (src_format != dst_format)
						{
							LOG_WARNING(RSX, "Sampling from a section of a render target, but formats might be incompatible (0x%X vs 0x%X)", src_format, dst_format);
						}
					}
					else
					{
						LOG_WARNING(RSX, "Surface blit from a compressed texture");
					}

					if (!rsc.is_bound || !g_cfg.video.strict_rendering_mode)
					{
						if (rsc.w == tex_width && rsc.h == tex_height)
						{
							if (rsc.is_bound)
							{
								LOG_WARNING(RSX, "Sampling from a currently bound render target @ 0x%x", texaddr);

								auto &caps = gl::get_driver_caps();
								if (caps.ARB_texture_barrier_supported)
									glTextureBarrier();
								else if (caps.NV_texture_barrier_supported)
									glTextureBarrierNV();
							}

							rsc.surface->bind();
						}
						else
							bound_index = create_temporary_subresource(rsc.surface->id(), (GLenum)rsc.surface->get_compatible_internal_format(), rsc.x, rsc.y, rsc.w, rsc.h);
					}
					else
					{
						LOG_WARNING(RSX, "Attempting to sample a currently bound render target @ 0x%x", texaddr);
						bound_index = create_temporary_subresource(rsc.surface->id(), (GLenum)rsc.surface->get_compatible_internal_format(), rsc.x, rsc.y, rsc.w, rsc.h);
					}

					if (bound_index)
						return;
				}
			}

			/**
			 * If all the above failed, then its probably a generic texture.
			 * Search in cache and upload/bind
			 */

			cached_texture_section *cached_texture = find_texture_from_dimensions(texaddr, tex_width, tex_height);
			if (cached_texture)
			{
				verify(HERE), cached_texture->is_empty() == false;

				gl_texture.set_id(cached_texture->id());
				gl_texture.bind();

				//external gl::texture objects should always be undefined/uninitialized!
				gl_texture.set_id(0);
				return;
			}

			/**
			 * Check for subslices from the cache in case we only have a subset a larger texture
			 */
			cached_texture = find_texture_from_range(texaddr, range);
			if (cached_texture)
			{
				const u32 address_offset = texaddr - cached_texture->get_section_base();
				const u32 format = tex.format() & ~(CELL_GCM_TEXTURE_LN | CELL_GCM_TEXTURE_UN);
				const GLenum ifmt = gl::get_sized_internal_format(format);

				u16 offset_x = 0, offset_y = 0;

				if (address_offset)
				{
					const u32 bpp = get_format_block_size_in_bytes(format);

					offset_y = address_offset / tex_pitch;
					offset_x = address_offset % tex_pitch;

					offset_x /= bpp;
					offset_y /= bpp;
				}

				u32 texture_id = create_temporary_subresource(cached_texture->id(), ifmt, offset_x, offset_y, tex_width, tex_height);
				if (texture_id) return;
			}

			gl_texture.init(index, tex);

			cached_texture_section &cached = create_texture(gl_texture.id(), texaddr, (const u32)get_texture_size(tex), tex_width, tex_height);
			
			writer_lock lock(m_cache_mutex);
			cached.protect(utils::protection::ro);
			cached.set_dirty(false);

			//external gl::texture objects should always be undefined/uninitialized!
			gl_texture.set_id(0);
		}

		bool upload_scaled_image(rsx::blit_src_info& src, rsx::blit_dst_info& dst, bool interpolate, gl_render_targets &m_rtts)
		{
			//Since we will have dst in vram, we can 'safely' ignore the swizzle flag
			//TODO: Verify correct behavior
			bool is_depth_blit = false;
			bool src_is_render_target = false;
			bool dst_is_render_target = false;
			bool dst_is_argb8 = (dst.format == rsx::blit_engine::transfer_destination_format::a8r8g8b8);
			bool src_is_argb8 = (src.format == rsx::blit_engine::transfer_source_format::a8r8g8b8);

			GLenum src_gl_sized_format = src_is_argb8? GL_RGBA8: GL_RGB565;
			GLenum src_gl_format = src_is_argb8 ? GL_BGRA : GL_RGB;
			GLenum src_gl_type = src_is_argb8? GL_UNSIGNED_INT_8_8_8_8: GL_UNSIGNED_SHORT_5_6_5;

			u32 vram_texture = 0;
			u32 dest_texture = 0;

			const u32 src_address = (u32)((u64)src.pixels - (u64)vm::base(0));
			const u32 dst_address = (u32)((u64)dst.pixels - (u64)vm::base(0));

			//Account for rasterization offsets for RGBA render targets - RSX insets raster area by 8 or 16 pixels depending on compression mode
			const u32 src_raster_offset_x = src.compressed_x ? 16 : 8;
			const u32 src_raster_offset_y = src.compressed_y ? 16 : 8;
			const u32 dst_raster_offset_x = dst.compressed_x ? 16 : 8;
			const u32 dst_raster_offset_y = dst.compressed_y ? 16 : 8;

			u32 src_raster_offset = src_is_argb8 ? (src.pitch * src_raster_offset_y + src_raster_offset_x * 4) : 0;
			u32 dst_raster_offset = dst_is_argb8 ? (dst.pitch * dst_raster_offset_y + dst_raster_offset_y * 4) : 0;

			//Check if src/dst are parts of render targets
			auto dst_subres = m_rtts.get_surface_subresource_if_applicable(dst_address - dst_raster_offset, dst.width, dst.clip_height, dst.pitch, true, true, false);
			dst_is_render_target = dst_subres.surface != nullptr;

			//TODO: Handle cases where src or dst can be a depth texture while the other is a color texture - requires a render pass to emulate
			auto src_subres = m_rtts.get_surface_subresource_if_applicable(src_address - src_raster_offset, src.width, src.height, src.pitch, true, true, false);
			src_is_render_target = src_subres.surface != nullptr;

			//Always use GPU blit if src or dst is in the surface store
			if (!g_cfg.video.use_gpu_texture_scaling && !(src_is_render_target || dst_is_render_target))
				return false;

			u16 max_dst_width = dst.width;
			u16 max_dst_height = dst.height;

			//Prepare areas and offsets
			//Copy from [src.offset_x, src.offset_y] a region of [clip.width, clip.height]
			//Stretch onto [dst.offset_x, y] with clipping performed on the source region
			//The implementation here adds the inverse scaled clip dimensions onto the source to completely bypass final clipping step
			float scale_x = dst.scale_x;
			float scale_y = dst.scale_y;

			//Clip offset is unused if the clip offsets are reprojected onto the source
			position2i clip_offset = { 0, 0 };//{ dst.clip_x, dst.clip_y };
			position2i dst_offset = { dst.offset_x, dst.offset_y };

			size2i clip_dimensions = { dst.clip_width, dst.clip_height };
			//Dimensions passed are restricted to powers of 2; get real height from clip_height and width from pitch
			size2i dst_dimensions = { dst.pitch / (dst_is_argb8 ? 4 : 2), dst.clip_height };

			//Offset in x and y for src is 0 (it is already accounted for when getting pixels_src)
			//Reproject final clip onto source...
			const u16 src_w = (const u16)((f32)clip_dimensions.width / dst.scale_x);
			const u16 src_h = (const u16)((f32)clip_dimensions.height / dst.scale_y);

			areai src_area = { 0, 0, src_w, src_h };
			areai dst_area = { 0, 0, dst.clip_width, dst.clip_height };

			//If destination is neither a render target nor an existing texture in VRAM
			//its possible that this method is being used to perform a memcpy into RSX memory, so we check
			//parameters. Whenever a simple memcpy can get the job done, use it instead.
			//Dai-3-ji Super Robot Taisen for example uses this to copy program code to GPU RAM
			
			bool is_memcpy = false;
			u32 memcpy_bytes_length = 0;
			
			if (dst_is_argb8 == src_is_argb8 && !dst.swizzled)
			{
				if ((src.slice_h == 1 && dst.clip_height == 1) ||
					(dst.clip_width == src.width && dst.clip_height == src.slice_h && src.pitch == dst.pitch))
				{
					const u8 bpp = dst_is_argb8 ? 4 : 2;
					is_memcpy = true;
					memcpy_bytes_length = dst.clip_width * bpp * dst.clip_height;
				}
			}

			cached_texture_section* cached_dest = nullptr;
			if (!dst_is_render_target)
			{
				//First check if this surface exists in VRAM with exact dimensions
				//Since scaled GPU resources are not invalidated by the CPU, we need to reuse older surfaces if possible
				cached_dest = find_texture_from_dimensions(dst.rsx_address, dst_dimensions.width, dst_dimensions.height);

				//Check for any available region that will fit this one
				if (!cached_dest) cached_dest = find_texture_from_range(dst.rsx_address, dst.pitch * dst.clip_height);

				if (cached_dest)
				{
					//TODO: Verify that the new surface will fit
					dest_texture = cached_dest->id();

					//TODO: Move this code into utils since it is used alot
					const u32 address_offset = dst.rsx_address - cached_dest->get_section_base();

					const u16 bpp = dst_is_argb8 ? 4 : 2;
					const u16 offset_y = address_offset / dst.pitch;
					const u16 offset_x = address_offset % dst.pitch;

					dst_offset.x += offset_x / bpp;
					dst_offset.y += offset_y;

					max_dst_width = cached_dest->get_width();
					max_dst_height = cached_dest->get_height();
				}
				else if (is_memcpy)
				{
					memcpy(dst.pixels, src.pixels, memcpy_bytes_length);
					return true;
				}
			}
			else
			{
				dst_offset.x = dst_subres.x;
				dst_offset.y = dst_subres.y;

				dest_texture = dst_subres.surface->id();

				auto dims = dst_subres.surface->get_dimensions();
				max_dst_width = dims.first;
				max_dst_height = dims.second;

				if (is_memcpy)
				{
					//Some render target descriptions are actually invalid
					//Confirm this is a flushable RTT
					const auto rsx_pitch = dst_subres.surface->get_rsx_pitch();
					const auto native_pitch = dst_subres.surface->get_native_pitch();

					if (rsx_pitch <= 64 && native_pitch != rsx_pitch)
					{
						memcpy(dst.pixels, src.pixels, memcpy_bytes_length);
						return true;
					}
				}
			}

			//Create source texture if does not exist
			if (!src_is_render_target)
			{
				auto preloaded_texture = find_texture_from_dimensions(src_address, src.width, src.slice_h);

				if (preloaded_texture != nullptr)
				{
					vram_texture = preloaded_texture->id();
				}
				else
				{
					flush_address(src_address);

					GLboolean swap_bytes = !src_is_argb8;
					if (dst.swizzled)
					{
						//TODO: Check results against 565 textures
						if (src_is_argb8)
						{
							src_gl_format = GL_RGBA;
							swap_bytes = true;
						}
						else
						{
							LOG_ERROR(RSX, "RGB565 swizzled texture upload found");
						}
					}

					glGenTextures(1, &vram_texture);
					glBindTexture(GL_TEXTURE_2D, vram_texture);
					glTexStorage2D(GL_TEXTURE_2D, 1, src_gl_sized_format, src.width, src.slice_h);
					glPixelStorei(GL_UNPACK_ROW_LENGTH, src.pitch / (src_is_argb8 ? 4 : 2));
					glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
					glPixelStorei(GL_UNPACK_SWAP_BYTES, swap_bytes);
					glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
					glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
					glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, src.width, src.slice_h, src_gl_format, src_gl_type, src.pixels);
					
					auto &section = create_texture(vram_texture, src_address, src.pitch * src.slice_h, src.width, src.slice_h);
					writer_lock lock(m_cache_mutex);

					section.protect(utils::protection::ro);
					section.set_dirty(false);
				}
			}
			else
			{
				if (src_subres.w != clip_dimensions.width ||
					src_subres.h != clip_dimensions.height)
				{
					f32 subres_scaling_x = (f32)src.pitch / src_subres.surface->get_native_pitch();
					
					dst_area.x2 = (int)(src_subres.w * dst.scale_x * subres_scaling_x);
					dst_area.y2 = (int)(src_subres.h * dst.scale_y);
				}

				src_area.x2 = src_subres.w;				
				src_area.y2 = src_subres.h;

				src_area.x1 += src_subres.x;
				src_area.x2 += src_subres.x;
				src_area.y1 += src_subres.y;
				src_area.y2 += src_subres.y;

				if (src.compressed_y)
				{
					dst_area.y1 *= 2;
					dst_area.y2 *= 2;

					dst_dimensions.height *= 2;
				}

				vram_texture = src_subres.surface->id();
			}

			bool format_mismatch = false;

			if (src_subres.is_depth_surface)
			{
				if (dest_texture)
				{
					if (dst_is_render_target && !dst_subres.is_depth_surface)
					{
						LOG_ERROR(RSX, "Depth->RGBA blit requested but not supported");
						return true;
					}

					GLenum internal_fmt;
					glBindTexture(GL_TEXTURE_2D, dest_texture);
					glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, (GLint*)&internal_fmt);

					if (internal_fmt != (GLenum)src_subres.surface->get_compatible_internal_format())
						format_mismatch = true;
				}

				is_depth_blit = true;
			}
			
			//TODO: Check for other types of format mismatch
			if (format_mismatch)
			{
				invalidate_range(cached_dest->get_section_base(), cached_dest->get_section_size());
				
				dest_texture = 0;
				cached_dest = nullptr;
			}

			//Validate clip offsets (Persona 4 Arena at 720p)
			//Check if can fit
			//NOTE: It is possible that the check is simpler (if (clip_x >= clip_width))
			//Needs verification
			if ((dst.offset_x + dst.clip_x + dst.clip_width) > max_dst_width) dst.clip_x = 0;
			if ((dst.offset_y + dst.clip_y + dst.clip_height) > max_dst_height) dst.clip_y = 0;

			if (dst.clip_x || dst.clip_y)
			{
				//Reproject clip offsets onto source
				const u16 scaled_clip_offset_x = (const u16)((f32)dst.clip_x / dst.scale_x);
				const u16 scaled_clip_offset_y = (const u16)((f32)dst.clip_y / dst.scale_y);

				src_area.x1 += scaled_clip_offset_x;
				src_area.x2 += scaled_clip_offset_x;
				src_area.y1 += scaled_clip_offset_y;
				src_area.y2 += scaled_clip_offset_y;
			}

			GLenum dst_format = (is_depth_blit) ? (GLenum)src_subres.surface->get_compatible_internal_format() : (dst_is_argb8) ? GL_RGBA8 : GL_RGB565;
			u32 texture_id = m_hw_blitter.scale_image(vram_texture, dest_texture, src_area, dst_area, dst_format, dst_offset, clip_offset,
					dst_dimensions, clip_dimensions, dst_is_argb8, is_depth_blit, interpolate);

			if (dest_texture)
				return true;

			//TODO: Verify if any titles ever scale into CPU memory. It defeats the purpose of uploading data to the GPU, but it could happen
			//If so, add this texture to the no_access queue not the read_only queue
			const u8 bpp = dst_is_argb8 ? 4 : 2;
			const u32 real_width = dst.pitch / bpp;
			cached_texture_section &cached = create_texture(texture_id, dst.rsx_address, dst.pitch * dst.clip_height, real_width, dst_dimensions.height);
			
			writer_lock lock(m_cache_mutex);
			
			//These textures are completely GPU resident so we dont watch for CPU access
			//There's no data to be fetched from the CPU
			//Its is possible for a title to attempt to read from the region, but the CPU path should be used in such cases
			cached.protect(utils::protection::ro);
			cached.set_dirty(false);
			cached.set_depth_flag(is_depth_blit);

			return true;
		}
	};
}
