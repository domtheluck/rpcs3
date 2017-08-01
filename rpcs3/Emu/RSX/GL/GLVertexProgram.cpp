#include "stdafx.h"
#include "Emu/System.h"

#include "GLVertexProgram.h"
#include "GLCommonDecompiler.h"
#include "../GCM.h"

#include <algorithm>

std::string GLVertexDecompilerThread::getFloatTypeName(size_t elementCount)
{
	return getFloatTypeNameImpl(elementCount);
}

std::string GLVertexDecompilerThread::getIntTypeName(size_t elementCount)
{
	return "ivec4";
}


std::string GLVertexDecompilerThread::getFunction(FUNCTION f)
{
	return getFunctionImpl(f);
}

std::string GLVertexDecompilerThread::compareFunction(COMPARE f, const std::string &Op0, const std::string &Op1)
{
	return compareFunctionImpl(f, Op0, Op1);
}

void GLVertexDecompilerThread::insertHeader(std::stringstream &OS)
{
	OS << "#version 430\n\n";
	OS << "layout(std140, binding = 0) uniform ScaleOffsetBuffer\n";
	OS << "{\n";
	OS << "	mat4 scaleOffsetMat;\n";
	OS << "	ivec4 userClipEnabled[2];\n";
	OS << "	vec4 userClipFactor[2];\n";
	OS << "};\n";
}

void GLVertexDecompilerThread::insertInputs(std::stringstream & OS, const std::vector<ParamType>& inputs)
{
	if (rsx_vertex_program.interleaved_input)
	{
		OS << "layout(location=0) uniform usamplerBuffer input_attribute_stream;\n";
		OS << "layout(location=1) uniform usamplerBuffer input_register_stream;\n";
	}

	std::vector<std::tuple<size_t, std::string>> input_data;
	for (const ParamType &PT : inputs)
	{
		for (const ParamItem &PI : PT.items)
		{
			input_data.push_back(std::make_tuple(PI.location, PI.name));
		}
	}

	/**
	 * Its is important that the locations are in the order that vertex attributes are expected.
	 * If order is not adhered to, channels may be swapped leading to corruption
	*/

	std::sort(input_data.begin(), input_data.end());

	int location = 2;
	for (const std::tuple<size_t, std::string>& item : input_data)
	{
		for (const ParamType &PT : inputs)
		{
			for (const ParamItem &PI : PT.items)
			{
				if (PI.name == std::get<1>(item))
				{
					bool is_int = false;
					for (const auto &attrib : rsx_vertex_program.rsx_vertex_inputs)
					{
						if (attrib.location == std::get<0>(item))
						{
							if (attrib.int_type || attrib.flags & GL_VP_SINT_MASK) is_int = true;
							break;
						}
					}

					//store location index
					input_locations[PI.name] = PI.location;

					std::string samplerType = is_int ? "isamplerBuffer" : "samplerBuffer";
					OS << "layout(location=" << location++ << ")" << "	uniform " << samplerType << " " << PI.name << "_buffer;\n";
				}
			}
		}
	}
}

void GLVertexDecompilerThread::insertConstants(std::stringstream & OS, const std::vector<ParamType> & constants)
{
	OS << "layout(std140, binding = 1) uniform VertexConstantsBuffer\n";
	OS << "{\n";
	OS << "	vec4 vc[468];\n";
	OS << "	uint transform_branch_bits;\n";
	OS << "	ivec4 input_attributes[16];\n";
	OS << "};\n\n";

	for (const ParamType &PT: constants)
	{
		for (const ParamItem &PI : PT.items)
		{
			if (PI.name == "vc[468]")
				continue;

			OS << "uniform " << PT.type << " " << PI.name << ";\n";
		}
	}
}

static const vertex_reg_info reg_table[] =
{
	{ "gl_Position", false, "dst_reg0", "", false },
	{ "diff_color", true, "dst_reg1", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_FRONTDIFFUSE },
	{ "spec_color", true, "dst_reg2", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_FRONTSPECULAR },
	//These are only present when back variants are specified, otherwise the default diff/spec color vars are for both front and back
	{ "front_diff_color", true, "dst_reg3", "", false },
	{ "front_spec_color", true, "dst_reg4", "", false },
	//Fog output shares a data source register with clip planes 0-2 so only declare when specified
	{ "fog_c", true, "dst_reg5", ".xxxx", true, "", "", "", true, CELL_GCM_ATTRIB_OUTPUT_MASK_FOG },
	//Warning: Always define all 3 clip plane groups together to avoid flickering with openGL
	{ "gl_ClipDistance[0]", false, "dst_reg5", ".y * userClipFactor[0].x", false, "userClipEnabled[0].x > 0", "0.5", "", true, CELL_GCM_ATTRIB_OUTPUT_MASK_UC0 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC1 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC2 },
	{ "gl_ClipDistance[1]", false, "dst_reg5", ".z * userClipFactor[0].y", false, "userClipEnabled[0].y > 0", "0.5", "", true, CELL_GCM_ATTRIB_OUTPUT_MASK_UC0 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC1 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC2 },
	{ "gl_ClipDistance[2]", false, "dst_reg5", ".w * userClipFactor[0].z", false, "userClipEnabled[0].z > 0", "0.5", "", true, CELL_GCM_ATTRIB_OUTPUT_MASK_UC0 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC1 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC2 },
	{ "gl_PointSize", false, "dst_reg6", ".x", false },
	{ "gl_ClipDistance[3]", false, "dst_reg6", ".y * userClipFactor[0].w", false, "userClipEnabled[0].w > 0", "0.5", "", true, CELL_GCM_ATTRIB_OUTPUT_MASK_UC3 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC4 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC5 },
	{ "gl_ClipDistance[4]", false, "dst_reg6", ".z * userClipFactor[1].x", false, "userClipEnabled[1].x > 0", "0.5", "", true, CELL_GCM_ATTRIB_OUTPUT_MASK_UC3 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC4 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC5 },
	{ "gl_ClipDistance[5]", false, "dst_reg6", ".w * userClipFactor[1].y", false, "userClipEnabled[1].y > 0", "0.5", "", true, CELL_GCM_ATTRIB_OUTPUT_MASK_UC3 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC4 | CELL_GCM_ATTRIB_OUTPUT_MASK_UC5 },
	{ "tc0", true, "dst_reg7", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX0 },
	{ "tc1", true, "dst_reg8", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX1 },
	{ "tc2", true, "dst_reg9", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX2 },
	{ "tc3", true, "dst_reg10", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX3 },
	{ "tc4", true, "dst_reg11", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX4 },
	{ "tc5", true, "dst_reg12", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX5 },
	{ "tc6", true, "dst_reg13", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX6 },
	{ "tc7", true, "dst_reg14", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX7 },
	{ "tc8", true, "dst_reg15", "", false, "", "", "", false, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX8 },
	{ "tc9", true, "dst_reg6", "", false, "", "", "", true, CELL_GCM_ATTRIB_OUTPUT_MASK_TEX9 }  // In this line, dst_reg6 is correct since dst_reg goes from 0 to 15.
};

void GLVertexDecompilerThread::insertOutputs(std::stringstream & OS, const std::vector<ParamType> & outputs)
{
	bool insert_front_diffuse = (rsx_vertex_program.output_mask & CELL_GCM_ATTRIB_OUTPUT_MASK_FRONTDIFFUSE) != 0;
	bool insert_back_diffuse = (rsx_vertex_program.output_mask & CELL_GCM_ATTRIB_OUTPUT_MASK_BACKDIFFUSE) != 0;

	bool insert_front_specular = (rsx_vertex_program.output_mask & CELL_GCM_ATTRIB_OUTPUT_MASK_FRONTSPECULAR) != 0;
	bool insert_back_specular = (rsx_vertex_program.output_mask & CELL_GCM_ATTRIB_OUTPUT_MASK_BACKSPECULAR) != 0;

	bool front_back_diffuse = (insert_back_diffuse && insert_front_diffuse);
	bool front_back_specular = (insert_back_specular && insert_front_specular);

	for (auto &i : reg_table)
	{
		if (m_parr.HasParam(PF_PARAM_NONE, "vec4", i.src_reg) && i.need_declare)
		{
			if (i.check_mask && (rsx_vertex_program.output_mask & i.check_mask_value) == 0)
				continue;

			if (i.name == "front_diff_color")
				insert_front_diffuse = false;

			if (i.name == "front_spec_color")
				insert_front_specular = false;

			std::string name = i.name;

			if (front_back_diffuse && name == "diff_color")
				name = "back_diff_color";

			if (front_back_specular && name == "spec_color")
				name = "back_spec_color";

			OS << "out vec4 " << name << ";\n";
		}
		else
		{
			//Mesa drivers are very strict on shader-stage matching
			//Force some outputs to be declared even if unused
            if (i.need_declare && (rsx_vertex_program.output_mask & i.check_mask_value) > 0)
			{
                OS << "out vec4 " << i.name << ";\n";
			}
		}
	}

	if (insert_back_diffuse && insert_front_diffuse)
		OS << "out vec4 front_diff_color;\n";

	if (insert_back_specular && insert_front_specular)
		OS << "out vec4 front_spec_color;\n";
}

namespace
{
	std::string expand_to_vec4(std::string value, u8 vector_size)
	{
		switch (vector_size)
		{
		case 2:
			return "vec4(" + value + ", " + value + ", 1., 1.)";
		case 3:
			return "vec4(" + value + ", " + value + ", " + value + ", 1.)";
		default:
			LOG_ERROR(RSX, "invalid vector size %d" HERE, vector_size);
		case 1:
		case 4:
			//Expand not required
			//In case its one component, read is swizzled as .xxxx (GOW1 loading screen)
			return value;
		}
	}

	void insert_vertex_input_fetch(std::stringstream& OS)
	{
		//Actually decode a vertex attribute from a raw byte stream
		OS << "struct attribute_desc\n";
		OS << "{\n";
		OS << "	int type;\n";
		OS << "	int attribute_size;\n";
		OS << "	int starting_offset;\n";
		OS << "	int stride;\n";
		OS << "	int swap_bytes;\n";
		OS << "	int is_register;\n";
		OS << "};\n\n";

		OS << "uint get_bits(uvec4 v, int swap)\n";
		OS << "{\n";
		OS << "	if (swap != 0) return (v.w | v.z << 8 | v.y << 16 | v.x << 24);\n";
		OS << "	return (v.x | v.y << 8 | v.z << 16 | v.w << 24);\n";
		OS << "}\n\n";

		OS << "uint get_bits(uvec2 v, int swap)\n";
		OS << "{\n";
		OS << "	if (swap != 0) return (v.y | v.x << 8);\n";
		OS << "	return (v.x | v.y << 8);\n";
		OS << "}\n\n";

		OS << "int preserve_sign_s16(uint bits)\n";
		OS << "{\n";
		OS << "	//convert raw 16 bit value into signed 32-bit integer counterpart\n";
		OS << "	uint sign = bits & 0x8000;\n";
		OS << "	if (sign != 0) return int(bits | 0xFFFF0000);\n";
		OS << "	return int(bits);\n";
		OS << "}\n\n";

		OS << "float convert_to_f32(uint bits)\n";
		OS << "{\n";
		OS << "	uint sign = (bits >> 31) & 1;\n";
		OS << "	uint exp = (bits >> 23) & 0xff;\n";
		OS << "	uint mantissa = bits & 0x7fffff;\n";
		OS << "	float base = (sign != 0)? -1.f: 1.f;\n";
		OS << "	base *= exp2(exp - 127);\n";
		OS << "	float scale = 0.f;\n\n";
		OS << "	for (int x = 0; x < 23; x++)\n";
		OS << "	{\n";
		OS << "		int inv = (22 - x);\n";
		OS << "		if ((mantissa & (1 << inv)) == 0) continue;\n";
		OS << "		scale += 1.f / pow(2.f, float(inv));\n";
		OS << "	}\n";
		OS << "	return base * scale;\n";
		OS << "}\n";

		OS << "#define get_s16(v, s) preserve_sign_s16(get_bits(v, s))\n\n";

		OS << "vec4 fetch_attribute(attribute_desc desc, int vertex_id, usamplerBuffer input_stream)\n";
		OS << "{\n";
		OS << "	vec4 result = vec4(0., 0., 0., 1.);\n";
		OS << "	vec4 scale = vec4(1.);\n";
		OS << "	uvec4 tmp;\n";
		OS << "	uint bits;\n";
		OS << "	bool reverse_order = false;\n";
		OS << "\n";
		OS << "	int first_byte = (vertex_id * desc.stride) + desc.starting_offset;\n";
		OS << "	for (int n = 0; n < desc.attribute_size; n++)\n";
		OS << "	{\n";
		OS << "		switch (desc.type)\n";
		OS << "		{\n";
		OS << "		case 0:\n";
		OS << "			//signed normalized 16-bit\n";
		OS << "			tmp[0] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[1] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			result[n] = get_s16(tmp.xy, desc.swap_bytes);\n";
		OS << "			scale[n] = 32767.;\n";
		OS << "			break;\n";
		OS << "		case 1:\n";
		OS << "			//float\n";
		OS << "			tmp[0] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[1] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[2] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[3] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			result[n] = uintBitsToFloat(get_bits(tmp, desc.swap_bytes));\n";
		OS << "			break;\n";
		OS << "		case 2:\n";
		OS << "			//half\n";
		OS << "			tmp[0] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[1] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			result[n] = unpackHalf2x16(uint(get_bits(tmp.xy, desc.swap_bytes))).x;\n";
		OS << "			break;\n";
		OS << "		case 3:\n";
		OS << "			//unsigned byte\n";
		OS << "			result[n] = texelFetch(input_stream, first_byte++).x / 255.;\n";
		OS << "			reverse_order = (desc.swap_bytes != 0);\n";
		OS << "			break;\n";
		OS << "		case 4:\n";
		OS << "			//signed word\n";
		OS << "			tmp[0] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[1] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			result[n] = get_s16(tmp.xy, desc.swap_bytes);\n";
		OS << "			break;\n";
		OS << "		case 5:\n";
		OS << "			//cmp\n";
		OS << "			tmp[0] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[1] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[2] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			tmp[3] = texelFetch(input_stream, first_byte++).x;\n";
		OS << "			bits = get_bits(tmp, desc.swap_bytes);\n";
		OS << "			result.x = preserve_sign_s16((bits & 0x7FF) << 5);\n";
		OS << "			result.y = preserve_sign_s16(((bits >> 11) & 0x7FF) << 5);\n";
		OS << "			result.z = preserve_sign_s16(((bits >> 22) & 0x3FF) << 6);\n";
		OS << "			result.w = 1.;\n";
		OS << "			scale = vec4(32767., 32767., 32767., 1.);\n";
		OS << "			break;\n";
		OS << "		case 6:\n";
		OS << "			//ub256\n";
		OS << "			result[n] = float(texelFetch(input_stream, first_byte++).x);\n";
		OS << "			reverse_order = (desc.swap_bytes != 0);\n";
		OS << "			break;\n";
		OS << "		}\n";
		OS << "	}\n\n";
		OS << "	result /= scale;\n";
		OS << "	return (reverse_order)? result.wzyx: result;\n";
		OS << "}\n\n";

		OS << "attribute_desc fetch_desc(int location)\n";
		OS << "{\n";
		OS << "	attribute_desc result;\n";
		OS << "	result.type = input_attributes[location].x;\n";
		OS << "	result.attribute_size = input_attributes[location].y;\n";
		OS << "	result.starting_offset = input_attributes[location].z;\n";
		OS << "	result.stride = input_attributes[location].w & 0xFF;\n";
		OS << "	result.swap_bytes = (input_attributes[location].w >> 16) & 0x1;\n";
		OS << "	result.is_register = (input_attributes[location].w >> 17) & 0x1;\n";
		OS << "	return result;\n";
		OS << "}\n\n";

		OS << "vec4 read_location(int location, int vertex_id)\n";
		OS << "{\n";
		OS << "	attribute_desc desc = fetch_desc(location);\n";
		OS << "	if (desc.is_register != 0)\n";
		OS << "		return fetch_attribute(desc, vertex_id, input_register_stream);\n";
		OS << "	else\n";
		OS << "		return fetch_attribute(desc, vertex_id, input_attribute_stream);\n";
		OS << "}\n\n";
	}

	void add_input_interleaved(std::stringstream& OS, const ParamItem &PI, const std::vector<rsx_vertex_input> &inputs, std::unordered_map<std::string, int>& locations, bool arrays_only)
	{
		for (const auto &real_input : inputs)
		{
			if (real_input.location != PI.location)
				continue;

			if (arrays_only && !real_input.is_array)
				return;

			//Get vertex index
			std::string vertex_id = "gl_VertexID";

			if (!real_input.is_array)
				vertex_id = "0";
			else if (real_input.frequency > 1)
			{
				std::string divisor = std::to_string(real_input.frequency);

				if (real_input.is_modulo)
					vertex_id = "gl_VertexID % " + divisor;
				else
					vertex_id = "gl_VertexID / " + divisor;
			}

			OS << "	vec4 " << PI.name << " = read_location(" << locations[PI.name] << ", " << vertex_id << ");\n";
			return;
		}

		if (arrays_only)
			return;

		LOG_WARNING(RSX, "Vertex input %s does not have a matching vertex_input declaration", PI.name.c_str());
		OS << "	vec4 " << PI.name << "= vec4(0.);\n";
	}

	void add_input(std::stringstream & OS, const ParamItem &PI, const std::vector<rsx_vertex_input> &inputs, bool skip_arrays)
	{
		for (const auto &real_input : inputs)
		{
			if (real_input.location != PI.location)
				continue;

			if (real_input.is_array && skip_arrays)
				return;

			std::string vecType = "	vec4 ";
			if (real_input.int_type)
				vecType = "	ivec4 ";

			std::string scale = "";
			if (real_input.flags & GL_VP_SINT_MASK)
			{
				if (real_input.flags & GL_VP_ATTRIB_S16_INT)
					scale = " / " + expand_to_vec4("32767.", real_input.size);
				else
					scale = " / " + expand_to_vec4("2147483647.", real_input.size);
			}

			if (!real_input.is_array)
			{
				OS << vecType << PI.name << " = texelFetch(" << PI.name << "_buffer, 0)" << scale << ";\n";
				return;
			}

			if (real_input.frequency > 1)
			{
				if (real_input.is_modulo)
				{
					OS << vecType << PI.name << "= texelFetch(" << PI.name << "_buffer, gl_VertexID %" << real_input.frequency << ")" << scale << ";\n";
					return;
				}

				OS << vecType << PI.name << "= texelFetch(" << PI.name << "_buffer, gl_VertexID /" << real_input.frequency << ")" << scale << ";\n";
				return;
			}

			OS << vecType << PI.name << "= texelFetch(" << PI.name << "_buffer, gl_VertexID)" << scale << ";\n";
			return;
		}

		LOG_WARNING(RSX, "Vertex input %s does not have a matching vertex_input declaration", PI.name.c_str());
		OS << "	vec4 " << PI.name << "= texelFetch(" << PI.name << "_buffer, gl_VertexID);\n";
	}
}

void GLVertexDecompilerThread::insertMainStart(std::stringstream & OS)
{
	insert_glsl_legacy_function(OS, gl::glsl::glsl_vertex_program);

	if (rsx_vertex_program.interleaved_input)
		insert_vertex_input_fetch(OS);

	std::string parameters = "";
	for (int i = 0; i < 16; ++i)
	{
		std::string reg_name = "dst_reg" + std::to_string(i);
		if (m_parr.HasParam(PF_PARAM_NONE, "vec4", reg_name))
		{
			if (parameters.length())
				parameters += ", ";

			parameters += "inout vec4 " + reg_name;
		}
	}

	OS << "void vs_main(" << parameters << ")\n";
	OS << "{\n";

	//Declare temporary registers, ignoring those mapped to outputs
	for (const ParamType PT : m_parr.params[PF_PARAM_NONE])
	{
		for (const ParamItem &PI : PT.items)
		{
			if (PI.name.substr(0, 7) == "dst_reg")
				continue;

			OS << "	" << PT.type << " " << PI.name;
			if (!PI.value.empty())
				OS << " = " << PI.value;

			OS << ";\n";
		}
	}

	for (const ParamType &PT : m_parr.params[PF_PARAM_IN])
	{
		for (const ParamItem &PI : PT.items)
		{
			//TODO: Ability to interleave groups of inputs
			if (rsx_vertex_program.interleaved_input)
			{
				add_input_interleaved(OS, PI, rsx_vertex_program.rsx_vertex_inputs, input_locations, true);
				add_input(OS, PI, rsx_vertex_program.rsx_vertex_inputs, true);
			}
			else
				add_input(OS, PI, rsx_vertex_program.rsx_vertex_inputs, false);
		}
	}

	for (const ParamType &PT : m_parr.params[PF_PARAM_UNIFORM])
	{
		if (PT.type == "sampler2D")
		{
			for (const ParamItem &PI : PT.items)
			{
				OS << "	vec2 " << PI.name << "_coord_scale = vec2(1.);\n";
			}
		}
	}
}

void GLVertexDecompilerThread::insertMainEnd(std::stringstream & OS)
{
	OS << "}\n\n";

	OS << "void main ()\n";
	OS << "{\n";

	std::string parameters = "";

	if (ParamType *vec4Types = m_parr.SearchParam(PF_PARAM_NONE, "vec4"))
	{
		for (int i = 0; i < 16; ++i)
		{
			std::string reg_name = "dst_reg" + std::to_string(i);
			for (auto &PI : vec4Types->items)
			{
				if (reg_name == PI.name)
				{
					if (parameters.length())
						parameters += ", ";

					parameters += reg_name;
					OS << "	vec4 " << reg_name;

					if (!PI.value.empty())
						OS << "= " << PI.value;

					OS << ";\n";
				}
			}
		}
	}

	OS << "\n" << "	vs_main(" << parameters << ");\n\n";

	bool insert_front_diffuse = (rsx_vertex_program.output_mask & CELL_GCM_ATTRIB_OUTPUT_MASK_FRONTDIFFUSE) != 0;
	bool insert_front_specular = (rsx_vertex_program.output_mask & CELL_GCM_ATTRIB_OUTPUT_MASK_FRONTSPECULAR) != 0;

	bool insert_back_diffuse = (rsx_vertex_program.output_mask & CELL_GCM_ATTRIB_OUTPUT_MASK_BACKDIFFUSE) != 0;
	bool insert_back_specular = (rsx_vertex_program.output_mask & CELL_GCM_ATTRIB_OUTPUT_MASK_BACKSPECULAR) != 0;

	bool front_back_diffuse = (insert_back_diffuse && insert_front_diffuse);
	bool front_back_specular = (insert_back_specular && insert_front_specular);

	for (auto &i : reg_table)
	{
		if (m_parr.HasParam(PF_PARAM_NONE, "vec4", i.src_reg))
		{
			if (i.check_mask && (rsx_vertex_program.output_mask & i.check_mask_value) == 0)
				continue;

			if (i.name == "front_diff_color")
				insert_front_diffuse = false;

			if (i.name == "front_spec_color")
				insert_front_specular = false;

			std::string name = i.name;
			std::string condition = (!i.cond.empty()) ? "(" + i.cond + ") " : "";

			if (front_back_diffuse && name == "diff_color")
				name = "back_diff_color";

			if (front_back_specular && name == "spec_color")
				name = "back_spec_color";

			if (condition.empty() || i.default_val.empty())
			{
				if (!condition.empty()) condition = "if " + condition;
				OS << "	" << condition << name << " = " << i.src_reg << i.src_reg_mask << ";\n";
			}
			else
			{
				//Insert if-else condition
				OS << "	" << name << " = " << condition << "? " << i.src_reg << i.src_reg_mask << ": " << i.default_val << ";\n";
			}
		}
		else if (i.need_declare && (rsx_vertex_program.output_mask & i.check_mask_value) > 0)
		{
			//An output was declared but nothing was written to it
			//Set it to all ones (Atelier Escha)
			OS << "	" << i.name << " = vec4(1.);\n";
		}
	}

	if (insert_back_diffuse && insert_front_diffuse)
		if (m_parr.HasParam(PF_PARAM_NONE, "vec4", "dst_reg1"))
			OS << "	front_diff_color = dst_reg1;\n";

	if (insert_back_specular && insert_front_specular)
		if (m_parr.HasParam(PF_PARAM_NONE, "vec4", "dst_reg2"))
			OS << "	front_spec_color = dst_reg2;\n";

	OS << "	gl_Position = gl_Position * scaleOffsetMat;\n";

	//Since our clip_space is symetrical [-1, 1] we map it to linear space using the eqn:
	//ln = (clip * 2) - 1 to fully utilize the 0-1 range of the depth buffer
	//RSX matrices passed already map to the [0, 1] range but mapping to classic OGL requires that we undo this step
	//This can be made unnecessary using the call glClipControl(GL_LOWER_LEFT, GL_ZERO_TO_ONE).
	//However, ClipControl only made it to opengl core in ver 4.5 though, so this is a workaround.
	
	//NOTE: It is completely valid for games to use very large w values, causing the post-multiplied z to be in the hundreds
	//It is therefore critical that this step is done post-transform and the result re-scaled by w
	//SEE Naruto: UNS
	
	OS << "	float ndc_z = gl_Position.z / gl_Position.w;\n";
	OS << "	ndc_z = (ndc_z * 2.) - 1.;\n";
	OS << "	gl_Position.z = ndc_z * gl_Position.w;\n";
	OS << "}\n";
}


void GLVertexDecompilerThread::Task()
{
	m_shader = Decompile();
}

GLVertexProgram::GLVertexProgram()
{
}

GLVertexProgram::~GLVertexProgram()
{
	Delete();
}

void GLVertexProgram::Decompile(const RSXVertexProgram& prog)
{
	GLVertexDecompilerThread decompiler(prog, shader, parr);
	decompiler.Task();

	interleaved = prog.interleaved_input;
}

void GLVertexProgram::Compile()
{
	if (id)
	{
		glDeleteShader(id);
	}

	id = glCreateShader(GL_VERTEX_SHADER);

	const char* str = shader.c_str();
	const int strlen = ::narrow<int>(shader.length());

	fs::create_path(fs::get_config_dir() + "/shaderlog");
	fs::file(fs::get_config_dir() + "shaderlog/VertexProgram" + std::to_string(id) + ".glsl", fs::rewrite).write(str);

	glShaderSource(id, 1, &str, &strlen);
	glCompileShader(id);

	GLint r = GL_FALSE;
	glGetShaderiv(id, GL_COMPILE_STATUS, &r);
	if (r != GL_TRUE)
	{
		glGetShaderiv(id, GL_INFO_LOG_LENGTH, &r);

		if (r)
		{
			char* buf = new char[r + 1]();
			GLsizei len;
			glGetShaderInfoLog(id, r, &len, buf);
			LOG_ERROR(RSX, "Failed to compile vertex shader: %s", buf);
			delete[] buf;
		}

		LOG_NOTICE(RSX, "%s", shader.c_str());
		Emu.Pause();
	}
}

void GLVertexProgram::Delete()
{
	shader.clear();

	if (id)
	{
		if (Emu.IsStopped())
		{
			LOG_WARNING(RSX, "GLVertexProgram::Delete(): glDeleteShader(%d) avoided", id);
		}
		else
		{
			glDeleteShader(id);
		}
		id = 0;
	}
}
