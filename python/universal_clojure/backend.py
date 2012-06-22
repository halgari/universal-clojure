import json
import sys
import util.treadle as tr
from copy import copy

node_emitters = {}
const_emitters = {}

def node_emitter(f):
    node_emitters[f.__name__.replace("_", "-").replace("--compile-", "")] = f
    return f

def const_emitter(f):
    const_emitters[f.__name__.replace("_", "-").replace("--compile-", "")] = f
    return f

def compile_node(node, env):
    return node_emitters[node["node-type"]](node, env)

def compile_const(node, env):
    return const_emitters[node["data-type"]](node, env)

@node_emitter
def __compile_const(node, env):
    return compile_const(node, env)

@const_emitter
def __compile_string(node, env):
    return tr.Const(node["value"])

@const_emitter
def __compile_number(node, env):
    return tr.Const(node["value"])

@const_emitter
def __compile_bool(node, env):
    return tr.Const(node["value"])

@node_emitter
def __compile_do(node, env):
    return tr.Do(*map(lambda x: compile_node(x, env), node["body"]))

@node_emitter
def __compile_local(node, env):
    locname = node["name"]
    if node["local-type"] == "fn-arg":
        return env["fnargs"][node["offset"]]
    else:
        raise Exception("Unknown Local type: " + node["local-type"])

@node_emitter
def __compile_fn(node, env):
    form = node["forms"][0]
    args = form["args"]
    argexprs = map(lambda x: tr.Argument(str(x)), args)
    env = copy(env)
    env["fnargs"] = argexprs
    return tr.Func(argexprs, compile_node(form["body"], env), str(node["name"]))


@node_emitter
def __compile_vector_literal(node, env):
    if "items" in node:
        items = map(lambda x: compile_node(x, env), node["items"])
    else:
        items = []

    return tr.List(*items)

@node_emitter
def __compile_if(node, env):
    return tr.If(*map(lambda x: compile_node(node[x], env),
                      ["cond", "then", "else"]))


@node_emitter
def __compile_invoke(node, env):
    fn = compile_node(node["fn"], env)
    args = []
    for x in node["args"]:
        args.append(compile_node(x, env))

    return tr.Call(fn, *args)


@const_emitter
def __compile_nil(node, env):
    return tr.Const(None)
