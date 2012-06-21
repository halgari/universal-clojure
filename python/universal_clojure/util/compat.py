import platform, struct, dis
import types

version = "".join(platform.python_version_tuple()[:2])

for x in dis.opmap:
    globals()[x] = dis.opmap[x]


class _Cond_Jump26(object):
    def __init__(self, ctx):
        self.loc = ctx.stream.tell()
        self.ctx = ctx
        ctx.stream.write(struct.pack("=BHB", 0xFF, 0xFFFF, POP_TOP))

    def mark(self):
        self.jumpto = self.ctx.stream.tell()
        self.ctx.stream.seek(self.loc)
        self.ctx.stream.write(struct.pack("=BH", JUMP_IF_FALSE, self.jumpto - self.loc - 3))

        self.ctx.stream.seek(0, SEEK_END)
        self.ctx.stream.write(struct.pack("=B", POP_TOP))


class AbsoluteJump(object):
    def __init__(self, ctx, bc = JUMP_ABSOLUTE):
        self.bc = bc
        self.loc = ctx.stream.tell()
        self.ctx = ctx
        ctx.stream.write(struct.pack("=BH", self.bc, 0xFFFF))

    def mark(self):
        self.jumpto = self.ctx.stream.tell()
        self.ctx.stream.seek(self.loc)
        self.ctx.stream.write(struct.pack("=BH", self.bc, self.jumpto))

        self.ctx.stream.seek(0, SEEK_END)



class _Cond_Jump27(object):
    def __init__(self, ctx):
        self.loc = ctx.stream.tell()
        self.ctx = ctx
        ctx.stream.write(struct.pack("=BH", POP_JUMP_IF_FALSE, 0xFFFF))
        self.jumpto = 0

    def mark(self):
        self.jumpto = self.ctx.stream.tell()
        self.ctx.stream.seek(self.loc)

        self.ctx.stream.write(struct.pack("=BH", POP_JUMP_IF_FALSE, self.jumpto))

        self.ctx.stream.seek(0, SEEK_END)


def newCode3(co_argcount = 0, co_nlocals = 0, co_stacksize = 0, co_flags = 0x0000,
            co_code = bytes(), co_consts = (), co_names = (), co_varnames = (),
            filename = "<string>", name = "", firstlineno = 0, co_lnotab = bytes(),
            co_freevars = (), co_cellvars = ()):
    """wrapper for CodeType so that we can remember the synatax"""
    return types.CodeType(co_argcount, 0, co_nlocals, co_stacksize,
        co_flags, co_code, co_consts, co_names, co_varnames,
        filename, name, firstlineno, co_lnotab, co_freevars, co_cellvars)

def newCode2(co_argcount = 0, co_nlocals = 0, co_stacksize = 0, co_flags = 0x0000,
             co_code = bytes(), co_consts = (), co_names = (), co_varnames = (),
             filename = "<string>", name = "", firstlineno = 0, co_lnotab = bytes(),
             co_freevars = (), co_cellvars = ()):
    """wrapper for CodeType so that we can remember the synatax"""
    return types.CodeType(co_argcount, co_nlocals, co_stacksize,
        co_flags, co_code, co_consts, co_names, co_varnames,
        filename, name, firstlineno, co_lnotab, co_freevars, co_cellvars)



if version == "26":
    CondJump = _Cond_Jump26
    from os import SEEK_END
else:
    CondJump = _Cond_Jump27
    from io import SEEK_END

if int(version) < 30:
    newCode = newCode2
else:
    newCode = newCode3
