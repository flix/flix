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
                <div className="title">Flix Library</div>
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
                <TypesBox/>
                <DefinitionsBox/>
                <div>
                    <h2>Relations</h2>
                    <h2>Lattices</h2>
                </div>
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

var DefinitionsBox = React.createClass({
    render: function () {
        return (
            <div className="definitions">
                <h2>Definitions</h2>
                {getData().definitions.map(d =>
                    <DefinitionItem name={d.name}
                                    d={d}/>
                )}
            </div>
        );
    }
});

var DefinitionItem = React.createClass({
    render: function () {
        return (
            <div>
                <b>def</b> {this.props.name} {this.props.d.params.map(kv => <span>{kv.name}</span>)}
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
        "definitions": [
            {
                "name": "minValue",
                "tparams": [],
                "params": [],
                "comment": "Returns the minimum number representable by an Int32."
            },
            {
                "name": "maxValue",
                "tparams": [],
                "params": [],
                "comment": "Returns the maximum number representable by an Int32."
            },
            {
                "name": "min",
                "tparams": [],
                "params": [{name: "x", tpe: "Int"}, {name: "y", tpe: "Int"}],
                "comment": "Returns the smaller of `x` and `y`."
            },
            {
                "name": "max",
                "tparams": [],
                "params": [],
                "comment": " Returns the larger of `x` and `y`."
            },
            {
                "name": "abs",
                "tparams": [],
                "params": [],
                "comment": "Returns the absolute value of `x`. If the absolute value exceeds maxValue(), -1 is returned"
            }
        ]
    }

}
