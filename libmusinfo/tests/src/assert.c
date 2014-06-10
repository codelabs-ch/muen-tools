/*
 * Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
 * Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

#include "musinfo.h"

int assert_name(const struct name_type * const name)
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
	if (sizeof(struct name_type) != size)
	{
		printf("Name: Invalid size %d /= %d\n", size, sizeof(struct name_type));
		return 0;
	}
	if (__alignof__ (struct name_type) != alignment)
	{
		printf("Name: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct name_type));
		return 0;
	}

	if (offsetof(struct name_type, length) != length_offset)
	{
		printf("Name: Invalid 'length' offset %d /= %d\n", length_offset,
				offsetof(struct name_type, length));
		return 0;
	}

	if (offsetof(struct name_type, data) != data_offset)
	{
		printf("Name: Invalid 'data' offset %d /= %d\n", data_offset,
				offsetof(struct name_type, data));
		return 0;
	}

	return 1;
}

int assert_channel(const struct channel_type * const channel)
{
	if (!assert_name(&channel->name))
	{
		return 0;
	}

	if (channel->address != 0xdeadbeefcafefeed)
	{
		printf("Channel: Invalid address 0x%lx\n", channel->address);
		return 0;
	}

	if (channel->size != 0x8080ababcdcd9090)
	{
		printf("Channel: Invalid size 0x%lx\n", channel->size);
		return 0;
	}

	if (!(channel->flags & WRITABLE_FLAG))
	{
		printf("Channel: Writable flag not set\n");
		return 0;
	}
	if (!(channel->flags & HAS_EVENT_FLAG))
	{
		printf("Channel: Has_Event flag not set\n");
		return 0;
	}
	if (!(channel->flags & HAS_VECTOR_FLAG))
	{
		printf("Channel: Has_Vector flag not set\n");
		return 0;
	}

	if (channel->event != 128)
	{
		printf("Channel: Invalid event number %d\n", channel->event);
		return 0;
	}

	if (channel->vector != 255)
	{
		printf("Channel: Invalid vector number %d\n", channel->vector);
		return 0;
	}

	return 1;
}

int assert_channel_type(const int size, const int alignment,
		const int name_offset, const int address_offset, const int size_offset,
		const int flags_offset, const int event_offset, const int vector_offset)
{
	if (sizeof(struct channel_type) != size)
	{
		printf("Channel: Invalid size %d /= %d\n", size,
				sizeof(struct channel_type));
		return 0;
	}
	if (__alignof__ (struct channel_type) != alignment)
	{
		printf("Channel: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct channel_type));
		return 0;
	}

	if (offsetof(struct channel_type, name) != name_offset)
	{
		printf("Channel: Invalid 'name' offset %d /= %d\n", name_offset,
				offsetof(struct channel_type, name));
		return 0;
	}

	if (offsetof(struct channel_type, address) != address_offset)
	{
		printf("Channel: Invalid 'address' offset %d /= %d\n", address_offset,
				offsetof(struct channel_type, address));
		return 0;
	}

	if (offsetof(struct channel_type, size) != size_offset)
	{
		printf("Channel: Invalid 'size' offset %d /= %d\n", size_offset,
				offsetof(struct channel_type, size));
		return 0;
	}

	if (offsetof(struct channel_type, flags) != flags_offset)
	{
		printf("Channel: Invalid 'flags' offset %d /= %d\n", flags_offset,
				offsetof(struct channel_type, flags));
		return 0;
	}

	if (offsetof(struct channel_type, event) != event_offset)
	{
		printf("Channel: Invalid 'event' offset %d /= %d\n", event_offset,
				offsetof(struct channel_type, event));
		return 0;
	}

	if (offsetof(struct channel_type, vector) != vector_offset)
	{
		printf("Channel: Invalid 'vector' offset %d /= %d\n", vector_offset,
				offsetof(struct channel_type, vector));
		return 0;
	}

	return 1;
}

int assert_subject_info(const struct subject_info_type * const info)
{
	if (info->magic != MUEN_SUBJECT_INFO_MAGIC)
	{
		printf("Sinfo: Invalid magic '%lx'\n", info->magic);
		return 0;
	}

	if (info->channel_count != MAX_CHANNEL_COUNT)
	{
		printf("Sinfo: Invalid channel count %d\n", info->channel_count);
		return 0;
	}

	int i;
	for (i = 0; i < info->channel_count; i++)
	{
		if (!assert_channel(&info->channels[i]))
		{
			return 0;
		}
	}

	return 1;
}
