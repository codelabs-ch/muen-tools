#!/usr/bin/env python3
#
# Print CSL commands in given image.

import argparse
from io import BufferedWriter
import os
import sys
import struct

CSL_VERMAGIC = struct.pack("Q", 0x8ADC5FA2448CB65E)

CMDS = ["CMD_WRITE", "CMD_FILL", "CMD_SET_ENTRY_POINT", "CMD_CHECK_CPUID"]


def parse_args():
    """
    Returned parsed command line arguments
    """
    arg_parser = argparse.ArgumentParser(description="CSL decoder")
    arg_parser.add_argument("img_src", type=str, help="CSL image source")
    arg_parser.add_argument(
        "-e", "--extract", type=str, help=("extract data to given path")
    )
    return arg_parser.parse_args()


# info output


def hex_dump(data: bytes, width: int = 16) -> None:
    for i in range(0, len(data), width):
        chunk = data[i : i + width]
        hex_chunk = " ".join(f"{byte:02x}" for byte in chunk)
        ascii_chunk = " ".join(
            chr(byte) if 32 <= byte <= 127 else "." for byte in chunk
        )
        print(f"{i:08x}  {hex_chunk:<{width*3}}  {ascii_chunk}")


def csl_write(name: str, data: bytes) -> None:
    if len(data) < 9:
        raise ValueError(f"{name}: Invalid data length {len(data)}")

    addr = int.from_bytes(data[0:7], "little")
    content_len = len(data) - 8

    print(f"addr {hex(addr)}, len {hex(content_len)}")


def csl_fill(name: str, data: bytes) -> None:
    if len(data) != 24:
        raise ValueError(f"{name}: Invalid data length {len(data)}")

    addr = int.from_bytes(data[0:8], "little")
    fill_length = int.from_bytes(data[8:16], "little")
    pattern = int.from_bytes(data[16:17])

    print(f"addr {hex(addr)}, len {hex(fill_length)}, pattern {hex(pattern)}")


def csl_set_entry_point(name: str, data: bytes) -> None:
    if len(data) != 8:
        raise ValueError(f"{name}: Invalid data length {len(data)}")
    addr = int.from_bytes(data[0:8], "little")
    print(f"addr {hex(addr)}")


def csl_check_cpuid(name: str, data: bytes):
    if len(data) != 88:
        raise ValueError(f"{name}: Invalid data length {len(data)}")

    ecx = int.from_bytes(data[0:4], "little")
    leaf = int.from_bytes(data[4:8], "little")
    value = int.from_bytes(data[8:12], "little")
    mask = int.from_bytes(data[12:16], "little")
    reg = int.from_bytes(data[16:17], "little")

    if reg not in range(0, 4):
        raise ValueError(f"Result register invalid: {reg}")

    msg = bytearray([byte for byte in data[24:] if byte != 0]).decode()
    print(
        f"ecx {ecx}\tleaf {hex(leaf)}\tvalue {hex(value)}\tmask {hex(mask)}\treg {reg}\tmsg '{msg}'"
    )


def csl_vendor_specific(name: str, data: bytes):
    print(f"data_len {hex(len(data))}")
    hex_dump(data)


# extract


def csl_write_file(name: str, data: bytes, out: BufferedWriter) -> None:
    if len(data) < 9:
        raise ValueError(f"{name}: Invalid data length {len(data)}")

    addr = int.from_bytes(data[0:7], "little")
    out.seek(addr)
    out.write(data)


def csl_fill_file(name: str, data: bytes, out: BufferedWriter) -> None:
    if len(data) != 24:
        raise ValueError(f"{name}: Invalid data length {len(data)}")

    addr = int.from_bytes(data[0:8], "little")
    fill_length = int.from_bytes(data[8:16], "little")
    pattern = data[16:17]

    out.seek(addr)
    out.write(pattern * fill_length)


args = parse_args()

if not os.path.isfile(args.img_src):
    sys.exit("Error: CSL source '" + args.img_src + "' not found")


def dispatch_info(cmd_str: str, data: bytes) -> None:
    match cmd_str:
        case "CMD_WRITE":
            csl_write(cmd_str, data)
        case "CMD_FILL":
            csl_fill(cmd_str, data)
        case "CMD_SET_ENTRY_POINT":
            csl_set_entry_point(cmd_str, data)
        case "CMD_CHECK_CPUID":
            csl_check_cpuid(cmd_str, data)
        case "VENDOR_SPECIFIC":
            csl_vendor_specific(cmd_str, data)


def dispatch_extract(cmd_str: str, data: bytes, out: BufferedWriter) -> None:
    match cmd_str:
        case "CMD_WRITE":
            csl_write_file(cmd_str, data, out)
        case "CMD_FILL":
            csl_fill_file(cmd_str, data, out)


with open(args.img_src, "rb") as img_src:
    magic = img_src.read(8)
    if magic != CSL_VERMAGIC:
        sys.exit("Error: source '" + args.img_src + "' is not a CSL image")

    out_file = None
    if args.extract:
        out_file = open(args.extract, "wb")

    while True:
        data = img_src.read(8)
        if len(data) != 8:
            break
        cmd = int.from_bytes(data, "little")

        if cmd <= len(CMDS):
            cmd_str = CMDS[cmd]
        elif cmd in range(60000, 65536):
            cmd_str = "VENDOR_SPECIFIC"
        else:
            raise ValueError(f"Invalid cmd ID {cmd}")

        data = img_src.read(8)
        if len(data) != 8:
            raise ValueError("Unable to read length")
        length = int.from_bytes(data, "little")

        data = img_src.read(length)
        if len(data) != length:
            raise ValueError("Unable to read cmd data")

        if out_file:
            dispatch_extract(cmd_str, data, out_file)
        else:
            print(f"{cmd_str:22}", end="")
            dispatch_info(cmd_str, data)

    if out_file:
        out_file.close()
