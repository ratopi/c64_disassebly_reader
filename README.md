# c64

An stupid simple converted from Hex-Assembly to binary image file for the
Commdore 64 files in https://github.com/mist64/c64disasm.

This is just for fun ... so don't blame me for anything.

Pull request are welcome ;-)

## Run

Start shell with

	rebar3 shell

and do a

	c64_read_dis:convert("c64disasm_ms.txt","basic.bin").

or a

	c64_read_dis:convert("c64disasm_cbm.txt","kernal.bin").
