var React = require('react');
var ReactDOM = require('react-dom');

/**
 * Main entry point.
 *
 * Renders the left navigation bar and the page content.
 */
var App = React.createClass({
    render: function () {
        return (
            <div className="app">
                <LeftNavigationBar/>
                <PageContent namespace={getData().namespace}/>
            </div>
        );
    }
});

/**
 * Renders the navigation bar which shows the list of namespaces in the library.
 */
var LeftNavigationBar = React.createClass({
    render: function () {
        // Construct a list item for each namespace in the library.
        var menu = getData().namespaces.map(
            namespace => <li key={namespace.name}><a href="#">{namespace.name}</a></li>
        );
        return (
            <div className="navbar">
                <div className="title">Flix Standard Library</div>
                <ul>{menu}</ul>
            </div>
        );
    }
});

/**
 * Renders the types, definitions, relations, and lattices declared in the current namespace.
 */
var PageContent = React.createClass({
    render: function () {
        return (
            <div className="pagecontent">
                <h1>{this.props.namespace}</h1>
                <TypeList decls={getData().types}/>
                <DefinitionList decls={getData().definitions}/>
                <RelationList decls={getData().relations}/>
                <LatticeList decls={getData().lattices}/>
            </div>
        );
    }
});

/**
 * Renders a list of type declarations.
 */
var TypeList = React.createClass({
    render: function () {
        var typeList = this.props.decls.map(d => <TypeBox key={d.name} decl={d}/>);
        if (typeList.length == 0)
            return null;
        return (
            <div className="type-list">
                <h2>Types</h2>
                {typeList}
            </div>
        );
    }
});

/**
 * Renders a single type declaration.
 */
var TypeBox = React.createClass({
    render: function () {
        var name = this.props.decl.name;
        var comment = this.props.decl.comment;
        return (
            <div className="type-decl">
                <div className="signature">
                    <span className="keyword">enum</span>
                    <span className="name">{name}</span>
                </div>
                <div className="comment">
                    {comment}
                </div>
            </div>
        );
    }
});

/**
 * Renders a list of definition declarations.
 */
var DefinitionList = React.createClass({
    render: function () {
        var definitionList = this.props.decls.map(d => <DefinitionBox key={d.name} decl={d}/>);
        if (definitionList.length == 0)
            return null
        return (
            <div className="definition-list">
                <h2>Definitions</h2>
                {definitionList}
            </div>
        );
    }
});

/**
 * Renders a single definition declaration.
 */
var DefinitionBox = React.createClass({
    render: function () {
        var name = this.props.decl.name;
        var tparams = surround(intersperse(this.props.decl.tparams.map(
            tparam => <span key={Math.random()}>{tparam.name}</span>
        ), ", "), "[", "]");
        var fparams = surround(intersperse(this.props.decl.fparams.map(
            fparam => <span key={Math.random()}>{fparam.name}: <span key={Math.random()}
                                                                     className="type">{fparam.tpe}</span></span>
        ), ", "), "(", ")");
        var result = this.props.decl.result;
        var comment = this.props.decl.comment;
        return (
            <div className="definition-decl">
                <div className="signature">
                    <span className="keyword">def</span>
                    <span className="name">{name}</span>
                    <span className="tparams">{tparams}</span>
                    <span className="fparams">{fparams}</span>
                    <span className="result">: <span className="type">{result}</span></span>
                </div>
                <div className="comment">
                    {comment}
                </div>
            </div>
        );
    }
});

/**
 * Renders a list of relation declarations.
 */
var RelationList = React.createClass({
    render: function () {
        var relationList = this.props.decls.map(d => <RelationBox key={d.name} decl={d}/>);
        if (relationList.length == 0)
            return null;
        return (
            <div className="relation-list">
                <h2>Relations</h2>
                {relationList}
            </div>
        );
    }
});

/**
 * Renders a single relation declaration.
 */
var RelationBox = React.createClass({
    render: function () {
        var name = this.props.decl.name;
        var attributes = surround(intersperse(this.props.decl.attributes.map(
            attr => <span key={Math.random()}>{attr.name}: <span key={Math.random()} className="type">{attr.tpe}</span></span>
        ), ", "), "(", ")");
        var comment = this.props.decl.comment;
        return (
            <div className="relation-decl">
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

/**
 * Renders a list of lattice declarations.
 */
var LatticeList = React.createClass({
    render: function () {
        var latticeList = this.props.decls.map(d => <LatticeBox key={d.name} decl={d}/>);
        if (latticeList.length == 0)
            return null
        return (
            <div className="lattice-list">
                <h2>Lattices</h2>
                {latticeList}
            </div>
        );
    }
});

/**
 * Renders a single lattice declaration.
 */
var LatticeBox = React.createClass({
    render: function () {
        var name = this.props.decl.name;
        var attributes = surround(intersperse(this.props.decl.attributes.map(
            attr => <span key={Math.random()}>{attr.name}: <span key={Math.random()} className="type">{attr.tpe}</span></span>
        ), ", "), "(", ")");
        var comment = this.props.decl.comment;
        return (
            <div className="lattice-decl">
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

/**
 * Render the page.
 */
ReactDOM.render(<App />, document.getElementById("app"));


// STUB DATA:

function getData() {

    return {
        namespace: "Root",

        namespaces: [
            {name: "BigInt"},
            {name: "Float32"},
            {name: "Float64"},
            {name: "Int8"},
            {name: "Int16"},
            {name: "Int32"},
            {name: "Int64"}
        ],

        types: [
            {
                name: "Color",
                cases: [{name: "Red", tpe: "Unit"}, {name: "Green", tpe: "Unit"}, {name: "Blue", tpe: "Unit"}],
                comment: "The colors of the rainbow."
            },
            {
                name: "Shape",
                cases: [{name: "Rectangle", tpe: "(Int, Int)"}, {name: "Circle", tpe: "Int"}],
                comment: "Different two-dimensional shapes."
            }
        ],
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
        "lattices": []
    }

}
