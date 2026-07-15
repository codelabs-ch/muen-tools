/*
 * Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
 * Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

#include "musinfo.h"

int assert_name(const struct muen_name_type *const name)
{
	if (name->length != MAX_NAME_LENGTH)
	{
		printf("Name: Invalid length %d\n", name->length);
		return 0;
	}

	int i;
	for (i = 0; i < name->length; i++)
	{
		if (name->data[i] != 'a')
		{
			printf("Name: Invalid character '%c' at position %d\n",
					name->data[i], i);
			return 0;
		}
	}
	return 1;
}

int assert_name_type(const int size, const int alignment,
		const int length_offset, const int data_offset)
{
	if (sizeof(struct muen_name_type) != size)
	{
		printf("Name: Invalid size %d /= %d\n", size,
				sizeof(struct muen_name_type));
		return 0;
	}
	if (__alignof__ (struct muen_name_type) != alignment)
	{
		printf("Name: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct muen_name_type));
		return 0;
	}

	if (offsetof(struct muen_name_type, length) != length_offset)
	{
		printf("Name: Invalid 'length' offset %d /= %d\n", length_offset,
				offsetof(struct muen_name_type, length));
		return 0;
	}

	if (offsetof(struct muen_name_type, data) != data_offset)
	{
		printf("Name: Invalid 'data' offset %d /= %d\n", data_offset,
				offsetof(struct muen_name_type, data));
		return 0;
	}

	return 1;
}

int assert_memregion(const struct muen_memregion_type *const memregion)
{
	int i;

	if (memregion->kind != MUEN_MEM_SUBJ_ZP)
	{
		printf("Memregion: Invalid kind 0x%u\n", memregion->kind);
		return 0;
	}

	if (memregion->content != MUEN_CONTENT_FILL)
	{
		printf("Memregion: Invalid content 0x%u\n", memregion->content);
		return 0;
	}

	if (memregion->address != 0xdeadbeefcafefeed)
	{
		printf("Memregion: Invalid address 0x%lx\n", memregion->address);
		return 0;
	}

	if (memregion->size != 0x8080ababcdcd9000)
	{
		printf("Memregion: Invalid size field 0x%lx\n", memregion->size);
		return 0;
	}

	for (i = 0; i < HASH_LENGTH; i++)
	{
		if (memregion->hash[i] != 253)
		{
			printf("Memregion: Invalid hash value %u at position %d\n",
					memregion->hash[i], i);
			return 0;
		}
	}

	if (!(memregion->flags & MEM_WRITABLE_FLAG))
	{
		printf("Memregion: Writable flag not set\n");
		return 0;
	}
	if (!(memregion->flags & MEM_EXECUTABLE_FLAG))
	{
		printf("Memregion: Executable flag not set\n");
		return 0;
	}

	if (memregion->pattern != 45)
	{
		printf("Memregion: Invalid pattern %u\n", memregion->pattern);
		return 0;
	}

	return 1;
}

int assert_memregion_type(const int size, const int kind_offset,
		const int content_offset, const int address_offset,
		const int size_offset, const int hash_offset,
		const int flags_offset, const int pattern_offset)
{
	if (sizeof(struct muen_memregion_type) != size)
	{
		printf("Memregion: Invalid struct size %d /= %d\n", size,
				sizeof(struct muen_memregion_type));
		return 0;
	}

	if (offsetof(struct muen_memregion_type, kind) != kind_offset)
	{
		printf("Memregion: Invalid 'kind' offset %d /= %d\n", kind_offset,
				offsetof(struct muen_memregion_type, kind));
		return 0;
	}

	if (offsetof(struct muen_memregion_type, content) != content_offset)
	{
		printf("Memregion: Invalid 'content' offset %d /= %d\n", content_offset,
				offsetof(struct muen_memregion_type, content));
		return 0;
	}

	if (offsetof(struct muen_memregion_type, address) != address_offset)
	{
		printf("Memregion: Invalid 'address' offset %d /= %d\n", address_offset,
				offsetof(struct muen_memregion_type, address));
		return 0;
	}

	if (offsetof(struct muen_memregion_type, size) != size_offset)
	{
		printf("Memregion: Invalid 'size' offset %d /= %d\n", size_offset,
				offsetof(struct muen_memregion_type, size));
		return 0;
	}

	if (offsetof(struct muen_memregion_type, hash) != hash_offset)
	{
		printf("Memregion: Invalid 'hash' offset %d /= %d\n", hash_offset,
				offsetof(struct muen_memregion_type, hash));
		return 0;
	}

	if (offsetof(struct muen_memregion_type, flags) != flags_offset)
	{
		printf("Memregion: Invalid 'flags' offset %d /= %d\n", flags_offset,
				offsetof(struct muen_memregion_type, flags));
		return 0;
	}

	if (offsetof(struct muen_memregion_type, pattern) != pattern_offset)
	{
		printf("Memregion: Invalid 'pattern' offset %d /= %d\n", pattern_offset,
				offsetof(struct muen_memregion_type, pattern));
		return 0;
	}

	return 1;
}

int assert_resource(const struct muen_resource_type *const resource)
{
	if (resource->kind != MUEN_RES_DEVICE)
	{
		printf("Resource: Invalid 'kind' %u /= RES_DEVICE\n", resource->kind);
		return 0;
	}

	if (!assert_name(&resource->name))
		return 0;

	return 1;
}

int assert_resource_type(const int size, const int alignment,
		const int name_offset, const int data_offset)
{
	if (sizeof(struct muen_resource_type) != size)
	{
		printf("Resource: Invalid size %d /= %d\n", size,
				sizeof(struct muen_resource_type));
		return 0;
	}
	if (__alignof__ (struct muen_resource_type) != alignment)
	{
		printf("Resource: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct muen_resource_type));
		return 0;
	}

	if (offsetof(struct muen_resource_type, name) != name_offset)
	{
		printf("Resource: Invalid 'name' offset %d /= %d\n", name_offset,
				offsetof(struct muen_resource_type, name));
		return 0;
    }

	if (offsetof(struct muen_resource_type, data) != data_offset)
	{
		printf("Resource: Invalid 'data' offset %d /= %d\n",
				data_offset,
				offsetof(struct muen_resource_type, data));
		return 0;
	}

	return 1;
}

int assert_device(const struct muen_device_type *const dev_info)
{
	if (dev_info->sid != 0xabcd)
	{
		printf("Dev: Invalid SID 0x%x\n", dev_info->sid);
		return 0;
	}

	if (dev_info->vendor_id != 0xfefe)
	{
		printf("Dev: Invalid vendor ID 0x%x\n", dev_info->vendor_id);
		return 0;
	}

	if (dev_info->device_id != 0x1234)
	{
		printf("Dev: Invalid device ID 0x%x\n", dev_info->device_id);
		return 0;
	}

	if (dev_info->class_code != 0xcece)
	{
		printf("Dev: Invalid class code 0x%x\n", dev_info->class_code);
		return 0;
	}

	if (dev_info->irte_start != 200)
	{
		printf("Dev: Invalid IRTE start %d\n", dev_info->irte_start);
		return 0;
	}

	if (dev_info->irq_start != 12)
	{
		printf("Dev: Invalid IRQ start %d\n", dev_info->irq_start);
		return 0;
	}

	if (dev_info->ir_count != 22)
	{
		printf("Dev: Invalid IR count %d\n", dev_info->ir_count);
		return 0;
	}

	if (!(dev_info->flags & DEV_MSI_FLAG))
	{
		printf("Dev: MSI flag not set\n");
		return 0;
	}

	if (dev_info->reset_method != MUEN_DEV_RESET_METHOD_BUS)
	{
		printf("Dev: Invalid reset method %d\n", dev_info->reset_method);
		return 0;
	}

	return 1;
}

int assert_device_type(const int size,
		const int sid_offset, const int vendor_id_offset, const int device_id_offset,
		const int class_code_offset, const int irte_start_offset,
		const int irq_start_offset, const int ir_count_offset,
		const int flags_offset, const int reset_method_offset)
{
	if (sizeof(struct muen_device_type) != size)
	{
		printf("Dev: Invalid size %d /= %d\n", size,
				sizeof(struct muen_device_type));
		return 0;
	}

	if (offsetof(struct muen_device_type, sid) != sid_offset)
	{
		printf("Dev: Invalid 'sid' offset %d /= %d\n", sid_offset,
				offsetof(struct muen_device_type, sid));
		return 0;
	}

	if (offsetof(struct muen_device_type, vendor_id) != vendor_id_offset)
	{
		printf("Dev: Invalid 'vendor_id' offset %d /= %d\n", vendor_id_offset,
				offsetof(struct muen_device_type, vendor_id));
		return 0;
	}

	if (offsetof(struct muen_device_type, device_id) != device_id_offset)
	{
		printf("Dev: Invalid 'device_id' offset %d /= %d\n", device_id_offset,
				offsetof(struct muen_device_type, device_id));
		return 0;
	}

	if (offsetof(struct muen_device_type, class_code) != class_code_offset)
	{
		printf("Dev: Invalid 'class_code' offset %d /= %d\n", class_code_offset,
				offsetof(struct muen_device_type, class_code));
		return 0;
	}

	if (offsetof(struct muen_device_type, irte_start) != irte_start_offset)
	{
		printf("Dev: Invalid 'irte_start' offset %d /= %d\n", irte_start_offset,
				offsetof(struct muen_device_type, irte_start));
		return 0;
	}

	if (offsetof(struct muen_device_type, irq_start) != irq_start_offset)
	{
		printf("Dev: Invalid 'irq_start' offset %d /= %d\n",
				irq_start_offset,
				offsetof(struct muen_device_type, irq_start));
		return 0;
	}

	if (offsetof(struct muen_device_type, ir_count) != ir_count_offset)
	{
		printf("Dev: Invalid 'ir_count' offset %d /= %d\n",
				ir_count_offset,
				offsetof(struct muen_device_type, ir_count));
		return 0;
	}

	if (offsetof(struct muen_device_type, flags) != flags_offset)
	{
		printf("Dev: Invalid 'flags' offset %d /= %d\n",
				flags_offset,
				offsetof(struct muen_device_type, flags));
		return 0;
	}

	if (offsetof(struct muen_device_type, reset_method) != reset_method_offset)
	{
		printf("Dev: Invalid 'reset_method' offset %d /= %d\n",
				reset_method_offset,
				offsetof(struct muen_device_type, reset_method));
		return 0;
	}

	return 1;
}

int assert_device_memory(const struct muen_devmem_type *const mem)
{
	if (mem->sid != 0xabcd)
	{
		printf("Devmem: Invalid SID 0x%x\n", mem->sid);
		return 0;
	}

	if (!(mem->flags & MEM_WRITABLE_FLAG))
	{
		printf("Devmem: Writable flag not set\n");
		return 0;
	}
	if (!(mem->flags & MEM_EXECUTABLE_FLAG))
	{
		printf("Devmem: Executable flag not set\n");
		return 0;
	}

	if (!(mem->bar_config.iomem_flags & DEVMEM_PREFETCHABLE_FLAG))
	{
		printf("Devmem: Prefetchable flag not set\n");
		return 0;
	}
	if (!(mem->bar_config.iomem_flags & DEVMEM_64BIT_FLAG))
	{
		printf("Devmem: 64-bit flag not set\n");
		return 0;
	}

	if (mem->bar_config.bar_idx != 5)
	{
		printf("Devmem: BAR index mismatch: %d\n", mem->bar_config.bar_idx);
		return 0;
	}

	if (mem->bar_config.bar_address != 0xabcdabcdabcdabcd)
	{
		printf("Devmem: BAR address mismatch: 0x%lx\n", mem->bar_config.bar_address);
		return 0;
	}

	if (mem->address != 0xdeadbeefcafefeed)
	{
		printf("Devmem: Invalid address 0x%lx\n", mem->address);
		return 0;
	}

	if (mem->size != 0x8080ababcdcd9000)
	{
		printf("Devmem: Invalid size field 0x%lx\n", mem->size);
		return 0;
	}

	return 1;
}

int assert_device_ioport(const struct muen_devport_type *const port)
{
	if (port->sid != 0xabcd)
	{
		printf("Devport: Invalid SID 0x%x\n", port->sid);
		return 0;
	}

	if (port->bar_idx != 5)
	{
		printf("Devport: BAR index mismatch: %d\n", port->bar_idx);
		return 0;
	}

	if (port->address != 0xfeed)
	{
		printf("Devport: Invalid address 0x%x\n", port->address);
		return 0;
	}

	if (port->size != 0x9000)
	{
		printf("Devport: Invalid size field 0x%x\n", port->size);
		return 0;
	}

	return 1;
}

int assert_device_memory_type(const int size, const int sid_offset,
		const int flags_offset, const int bar_config_offset,
		const int iomem_flags_offset, const int bar_idx_offset,
		const int bar_addr_offset, const int address_offset, const int size_offset)
{
	if (sizeof(struct muen_devmem_type) != size)
	{
		printf("Devmem: Invalid struct size %d /= %d\n", size,
				sizeof(struct muen_devmem_type));
		return 0;
	}

	if (offsetof(struct muen_devmem_type, sid) != sid_offset)
	{
		printf("Devmem: Invalid 'sid' offset %d /= %d\n", sid_offset,
				offsetof(struct muen_devmem_type, sid));
		return 0;
	}

	if (offsetof(struct muen_devmem_type, flags) != flags_offset)
	{
		printf("Devmem: Invalid 'flags' offset %d /= %d\n", flags_offset,
				offsetof(struct muen_devmem_type, flags));
		return 0;
	}

	if (offsetof(struct muen_devmem_type, bar_config) != bar_config_offset)
	{
		printf("Devmem: Invalid 'bar_config' offset %d /= %d\n", bar_config_offset,
				offsetof(struct muen_devmem_type, bar_config));
		return 0;
	}

	if (offsetof(struct muen_bar_config_type, iomem_flags) != iomem_flags_offset)
	{
		printf("Devmem: Invalid 'iomem_flags' offset %d /= %d\n", iomem_flags_offset,
				offsetof(struct muen_bar_config_type, iomem_flags));
		return 0;
	}

	if (offsetof(struct muen_bar_config_type, bar_idx) != bar_idx_offset)
	{
		printf("Devmem: Invalid 'bar_idx' offset %d /= %d\n", bar_idx_offset,
				offsetof(struct muen_bar_config_type, bar_idx));
		return 0;
	}

	if (offsetof(struct muen_bar_config_type, bar_address) != bar_addr_offset)
	{
		printf("Devmem: Invalid 'bar_address' offset %d /= %d\n", bar_addr_offset,
				offsetof(struct muen_bar_config_type, bar_address));
		return 0;
	}

	if (offsetof(struct muen_devmem_type, address) != address_offset)
	{
		printf("Devmem: Invalid 'address' offset %d /= %d\n", address_offset,
				offsetof(struct muen_devmem_type, address));
		return 0;
	}

	if (offsetof(struct muen_devmem_type, size) != size_offset)
	{
		printf("Devmem: Invalid 'size' offset %d /= %d\n", size_offset,
				offsetof(struct muen_devmem_type, size));
		return 0;
	}

	return 1;
}

int assert_device_ioport_type(const int size, const int sid_offset,
		const int bar_idx_offset, const int address_offset,
		const int size_offset)
{
	if (sizeof(struct muen_devport_type) != size)
	{
		printf("Devport: Invalid struct size %d /= %d\n", size,
				sizeof(struct muen_devport_type));
		return 0;
	}

	if (offsetof(struct muen_devport_type, sid) != sid_offset)
	{
		printf("Devport: Invalid 'sid' offset %d /= %d\n", sid_offset,
				offsetof(struct muen_devport_type, sid));
		return 0;
	}

	if (offsetof(struct muen_devport_type, bar_idx) != bar_idx_offset)
	{
		printf("Devport: Invalid 'bar_idx' offset %d /= %d\n", bar_idx_offset,
				offsetof(struct muen_devport_type, bar_idx));
		return 0;
	}

	if (offsetof(struct muen_devport_type, address) != address_offset)
	{
		printf("Devport: Invalid 'address' offset %d /= %d\n", address_offset,
				offsetof(struct muen_devport_type, address));
		return 0;
	}

	if (offsetof(struct muen_devport_type, size) != size_offset)
	{
		printf("Devport: Invalid 'size' offset %d /= %d\n", size_offset,
				offsetof(struct muen_devport_type, size));
		return 0;
	}

	return 1;
}

int assert_subject_info(const struct subject_info_type *const info)
{
	if (info->magic != MUEN_SUBJECT_INFO_MAGIC)
	{
		printf("Sinfo: Invalid magic '%lx'\n", info->magic);
		return 0;
	}

	if (!assert_name(&info->name))
		return 0;

	if (info->tsc_khz != 100000000) {
		printf("Sinfo: Invalid TSC value '%lx'\n", info->tsc_khz);
	}

	int i;
	for (i = 0; i < MAX_RESOURCE_COUNT; i++)
	{
		if (info->resources[i].kind != MUEN_RES_DEVICE)
		{
			printf("Sinfo: Resource at index %u not a device - %u\n",
					i, info->resources[i].kind);
			return 0;
		}
	}

	if (i != MAX_RESOURCE_COUNT)
	{
		printf("Sinfo: %u resources expected, only %u found\n", MAX_RESOURCE_COUNT, i);
		return 0;
	}

	return 1;
}

int assert_subject_info_type(const int size, const int alignment,
		const int magic_offset, const int tsc_khz_offset,
		const int name_offset, const int res_count_offset,
		const int resources_offset)
{
	if (sizeof(struct subject_info_type) != size)
	{
		printf("Sinfo: Invalid size %d /= %d\n", size,
				sizeof(struct subject_info_type));
		return 0;
	}
	if (__alignof__ (struct subject_info_type) != alignment)
	{
		printf("Sinfo: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct subject_info_type));
		return 0;
	}

	if (offsetof(struct subject_info_type, magic) != magic_offset)
	{
		printf("Sinfo: Invalid 'magic' offset %d /= %d\n", magic_offset,
				offsetof(struct subject_info_type, magic));
		return 0;
	}

	if (offsetof(struct subject_info_type, tsc_khz) != tsc_khz_offset)
	{
		printf("Sinfo: Invalid 'tsc_khz' offset %d /= %d\n", tsc_khz_offset,
				offsetof(struct subject_info_type, tsc_khz));
		return 0;
	}

	if (offsetof(struct subject_info_type, name) != name_offset)
	{
		printf("Sinfo: Invalid 'name' offset %d /= %d\n", name_offset,
				offsetof(struct subject_info_type, name));
		return 0;
	}

	if (offsetof(struct subject_info_type, resource_count) != res_count_offset)
	{
		printf("Sinfo: Invalid 'resource_count' offset %d /= %d\n", res_count_offset,
				offsetof(struct subject_info_type, resource_count));
		return 0;
	}

	if (offsetof(struct subject_info_type, resources) != resources_offset)
	{
		printf("Sinfo: Invalid 'resources' offset %d /= %d\n", resources_offset,
				offsetof(struct subject_info_type, resources));
		return 0;
	}

	return 1;
}
