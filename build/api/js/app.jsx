var React = require('react');
var ReactDOM = require('react-dom');

var App = React.createClass({
    render: function () {
        return (
            <div className="app">
                <Nav/>
                <MembersBox/>
            </div>
        );
    }
});

var Nav = React.createClass({
    render: function () {
        return (
            <div className="nav">
                <div className="title">Flix Standard Library</div>
                <ul>
                    <li><a href="#">BigInt</a></li>
                    <li><a href="#">Float32</a></li>
                    <li><a href="#">Float64</a></li>
                    <li><a href="#">Int8</a></li>
                    <li><a href="#">Int16</a></li>
                    <li><a href="#">Int32</a></li>
                    <li><a href="#">Int64</a></li>
                </ul>
            </div>
        );
    }
});

var MembersBox = React.createClass({
    render: function () {
        return (
            <div className="members">
                <h1>Root</h1>

                <TypesBox/>
                <DefinitionList/>
                <RelationList/>
                <h2>Lattices</h2>
            </div>
        );
    }
});

var TypesBox = React.createClass({
    render: function () {
        return (
            <div className="types">
                <h2>Types</h2>
                ...
            </div>
        );
    }
});

var DefinitionList = React.createClass({
    render: function () {
        return (
            <div className="definitions">

                <h2>Definitions</h2>

                {getData().definitions.map(d =>
                    <DefinitionBox d={d}/>
                )}
            </div>
        );
    }
});

var DefinitionBox = React.createClass({
    render: function () {
        var name = this.props.d.name;
        var tparams = surround(intersperse(this.props.d.tparams.map(
            tparam => <span>{tparam.name}</span>
        ), ", "), "[", "]");
        var fparams = surround(intersperse(this.props.d.fparams.map(
            fparam => <span>{fparam.name}: <span className="type">{fparam.tpe}</span></span>
        ), ", "), "(", ")");
        var result = this.props.d.result;
        var comment = this.props.d.comment;
        return (
            <div className="definition">
                <div className="signature">
                    <span className="keyword">def</span>
                    <span className="name">{name}</span>
                    <span className="tparams">{tparams}</span>
                    <span className="fparams">{fparams}</span>
                    <span className="result">: {result}</span>
                </div>
                <div className="comment">
                    {comment}
                </div>
            </div>
        );
    }
});


/**
 * Renders a list of relations.
 */
var RelationList = React.createClass({
    render: function () {
        return (
            <div className="Relations">
                <h2>Relations</h2>
                {getData().relations.map(r =>
                    <RelationBox r={r}/>
                )}
            </div>
        );
    }
});

/**
 * Renders a relation in a single box.
 */
var RelationBox = React.createClass({
    render: function () {
        var name = this.props.r.name;
        var attributes = surround(intersperse(this.props.r.attributes.map(
            attr => <span>{attr.name}: <span className="type">{attr.tpe}</span></span>
        ), ", "), "(", ")");
        var comment = this.props.r.comment;
        return (
            <div className="relation">
                <div className="signature">
                    <span className="keyword">rel</span>
                    <span className="name">{name}</span>
                    <span className="attributes">{attributes}</span>
                </div>
                <div className="comment">{comment}</div>
            </div>
        );
    }
});

ReactDOM.render(
    <App />,
    document.getElementById('body')
);


function getData() {

    return {
        types: [],
        "definitions": [
            {
                "name": "flatMap",
                "tparams": [
                    {name: "a"},
                    {name: "b"}
                ],
                "fparams": [
                    {name: "f", tpe: "a -> b"},
                    {name: "xs", tpe: "List[a]"}
                ],
                "result": "List[b]",
                "comment": "Applies the function f ..."
            },
            {
                "name": "minValue",
                "tparams": [],
                "fparams": [],
                "result": "Int",
                "comment": "Returns the minimum number representable by an Int32."
            },
            {
                "name": "maxValue",
                "tparams": [],
                "fparams": [],
                "result": "Int",
                "comment": "Returns the maximum number representable by an Int32."
            },
            {
                "name": "min",
                "tparams": [],
                "fparams": [{name: "x", tpe: "Int"}, {name: "y", tpe: "Int"}],
                "result": "Int",
                "comment": "Returns the smaller of `x` and `y`."
            },
            {
                "name": "max",
                "tparams": [],
                "fparams": [],
                "result": "Int",
                "comment": " Returns the larger of `x` and `y`."
            },
            {
                "name": "abs",
                "tparams": [],
                "fparams": [],
                "result": "Int",
                "comment": "Returns the absolute value of `x`. If the absolute value exceeds maxValue(), -1 is returned."
            }
        ],
        "relations": [
            {
                name: "VarPointsTo",
                attributes: [
                    {name: "c", tpe: "Ctx"},
                    {name: "s", tpe: "Stm"},
                    {name: "x", tpe: "Var"},
                    {name: "o", tpe: "Obj"}
                ],
                comment: "Var `v` points-to object `o` at statement `s` in context `c`."
            },
            {
                name: "HeapPointsToIn",
                attributes: [
                    {name: "c", tpe: "Ctx"},
                    {name: "s", tpe: "Stm"},
                    {name: "o1", tpe: "Obj"},
                    {name: "f", tpe: "Field"},
                    {name: "o2", tpe: "Obj"}
                ],
                comment: "Field `f` of object `o1` points-to object `o2` at statement `s` in context `c`."
            },
            {
                name: "PromiseStateIn",
                attributes: [
                    {name: "c", tpe: "Ctx"},
                    {name: "s", tpe: "Stm"},
                    {name: "t", tpe: "State"},
                    {name: "v", tpe: "Obj"}
                ],
                comment: "The promise `o` is in promise state `t` with value `v` at statement `s` in context `c`."
            }
        ],
        "lattices": [
            {
                name: "Val",
                attributes: [
                    {name: "c", tpe: "Ctx"},
                    {name: "s", tpe: "Stm"},
                    {name: "x", tpe: "Var"},
                    {name: "o", tpe: "Obj"}
                ],
                comment: "Var `v` points-to object `o` at statement `s` in context `c`."
            },
        ]
    }

}

/**
 * Returns the given array of elements surrounded by parenthesis (if non-empty).
 */
function surround(arr, b, e) {
    if (arr.length == 0)
        return arr;

    var result = [];
    result.push(b);
    arr.forEach(item => result.push(item));
    result.push(e);

    return result;
}

/**
 *  Return an array with the separator interspersed between each element of the input array.
 *
 * > _([1,2,3]).intersperse(0)
 * [1,0,2,0,3]
 *
 * http://stackoverflow.com/questions/23618744/rendering-comma-separated-list-of-links
 */
function intersperse(arr, sep) {
    if (arr.length === 0) {
        return [];
    }

    return arr.slice(1).reduce(function (xs, x, i) {
        return xs.concat([sep, x]);
    }, [arr[0]]);
}