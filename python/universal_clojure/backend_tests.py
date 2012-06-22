import backend
import json

with open("../test/universal_clojure/test/test_data.json") as f:
    nodes = json.load(f)


for x in nodes:
    form, result = x
    try:
        comp = backend.compile_node(form, {})
    except:
        print x
        raise
    assert comp.toFunc()() == result
