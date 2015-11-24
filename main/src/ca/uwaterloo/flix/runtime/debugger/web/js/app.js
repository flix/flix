var PointsTo = {
    cols: ["localVal", "value"],
    rows: [
        [1, "/ParityAnalysis::Parity.Odd(())"],
        [2, "/ParityAnalysis::Parity.Even(())"],
        [3, "/ParityAnalysis::Parity.Odd(())"],
        [7, "/ParityAnalysis::Parity.Odd(())"],
        [8, "/ParityAnalysis::Parity.Top(())"]
    ],
    align: ["left", "left", "left"]

};


var Rules = [
    {
        rule: "SUBefore(l2,a,t) :- CFG(l1,l2), SUAfter(l1,a,t).",
        hitcount: 959690,
        time: 8714,
        location: "101"
    },

    {
        rule: "SUAfter(l,a,t) :- SUBefore(l,a,t), Phi(l). ",
        hitcount: 9564,
        time: 97,
        location: "115"
    },

    {
        rule: "SUAfter(l,a,t) :- SUBefore(l,a,t), killNot(a, k), Kill(l,k). ",
        hitcount: 958449,
        time: 5562,
        location: "113"
    }
];


var Predicates = [
    {
        name: "Multi",
        size: 148,
        indexedLookups: 957011,
        indexedScans: 0,
        fullScans: 0
    },
    {
        name: "AddrOf",
        size: 915,
        indexedLookups: 915,
        indexedScans: 0,
        fullScans: 0
    },
    {
        name: "PtH",
        size: 811,
        indexedLookups: 218386,
        indexedScans: 0,
        fullScans: 225
    }
];

var Indexes = [
    {
        collection: "/Pt",
        index: "{variable, target}",
        hits: 2439278
    },
    {
        collection: "/CFG",
        index: "{label} ",
        hits: 959689
    },
    {
        collection: "/Phi",
        index: "{label} ",
        hits: 959565
    }
];


var Relations = [{
    "name": "/Multi",
    "size": 148
}, {
    "name": "/AddrOf",
    "size": 915
}, {
    "name": "/KillEmpty",
    "size": 0
}, {
    "name": "/PtSU",
    "size": 0
}, {
    "name": "/PtH",
    "size": 4
}, {
    "name": "/AllObjects",
    "size": 915
}, {
    "name": "/FILoad",
    "size": 8
}, {
    "name": "/Copy",
    "size": 481
}, {
    "name": "/Store",
    "size": 316
}, {
    "name": "/CFG",
    "size": 4525
}, {
    "name": "/Pt",
    "size": 1062
}, {
    "name": "/Clear",
    "size": 225
}, {
    "name": "/FIStore",
    "size": 69
}, {
    "name": "/Load",
    "size": 2139
}, {
    "name": "/Phi",
    "size": 2702
}]

var Lattices = [
    {name: "SUBefore", size: 609793},
    {name: "SUBefore", size: 605760},
    {name: "Kill", size: 1571}
];

var Snapshots = [
    {
        time: 1448397245,
        facts: 4325345,
        memory: 5654
    },
    {
        time: 1448397280,
        facts: 43257675,
        memory: 6654
    },
    {
        time: 1458397245,
        facts: 53257675,
        memory: 7654
    }
];

var Status = "completed";


var URL = "http://" + window.location.hostname + ":9090";

/**
 * Main Application entry point.
 */
var App = React.createClass({displayName: "App",

    getInitialState: function () {
        return {page: {name: "default"}, relations: []};
    },

    componentDidMount: function () {
        this.tick();
    },

    tick: function () {
        $.ajax({
            method: "GET", dataType: 'json', url: URL + '/relations', success: function (data) {
                this.setState({relations: data});
            }.bind(this),
            error: this.onError
        });
    },

    changePage: function (page) {
        this.setState({page: page})
    },


    onError: function (msg) {
        // TODO
        console.log(msg);
        alert(msg);
    },

    render: function () {
        var page = null;
        var pageName = this.state.page.name;

        if (pageName === "performance/rules") {
            page = React.createElement(RulesPage, null)
        } else if (pageName === "performance/predicates") {
            page = React.createElement(PredicatesPage, null)
        } else if (pageName === "performance/indexes") {
            page = React.createElement(IndexesPage, null)
        } else if (pageName === "compiler/phases") {
            page = React.createElement(PhasesPage, {notifyConnectionError: this.onError})
        } else if (pageName === "relation") {
            page = React.createElement(RelationPage, {name: "VarPointsTo", table: PointsTo})
        } else {
            page = React.createElement(LandingPage, {relations: this.state.relations, lattices: Lattices})
        }

        return (
            React.createElement("div", null, 
                React.createElement(Menu, {changePage: this.changePage, status: Status, relations: Relations, lattices: Lattices}), 
                page
            )
        );
    }
});

/**
 * Menu component.
 */
var Menu = React.createClass({displayName: "Menu",
    propTypes: {
        changePage: React.PropTypes.func.isRequired,
        relations: React.PropTypes.array.isRequired,
        lattices: React.PropTypes.array.isRequired
    },

    changePageRelation: function (relation) {
        return function () {
            this.props.changePage({name: "relation"})
        }.bind(this)
    },

    render: function () {
        return (
            React.createElement("nav", {className: "navbar navbar-default"}, 
                React.createElement("div", {className: "container-fluid"}, 
                    React.createElement("div", {className: "navbar-header"}, 
                        React.createElement("a", {className: "navbar-brand", href: "#", onClick: () => this.props.changePage({name: "default"})}, 
                            "Flix Debugger"
                        )
                    ), 

                    React.createElement("ul", {className: "nav navbar-nav"}, 
                        React.createElement("li", {className: "dropdown"}, 
                            React.createElement("a", {href: "#", className: "dropdown-toggle", "data-toggle": "dropdown"}, 
                                "Minimal Model ", React.createElement("span", {className: "caret"})
                            ), 
                            React.createElement("ul", {className: "dropdown-menu"}, 
                                this.props.relations.map(relation => {
                                    var name = relation.name;
                                    return React.createElement("li", {key: name, onClick: this.changePageRelation({name})}, 
                                        React.createElement("a", {href: "#"}, name)
                                    )
                                }), 

                                React.createElement("li", {role: "separator", className: "divider"}), 

                                this.props.lattices.map(lattice => {
                                    var name = lattice.name;
                                    return React.createElement("li", {key: name}, 
                                        React.createElement("a", {href: "#"}, name)
                                    )
                                })
                            )
                        )
                    ), 

                    React.createElement("ul", {className: "nav navbar-nav"}, 
                        React.createElement("li", {className: "dropdown"}, 
                            React.createElement("a", {href: "#", className: "dropdown-toggle", "data-toggle": "dropdown"}, 
                                "Performance", 
                                React.createElement("className", {className: "caret"})
                            ), 
                            React.createElement("ul", {className: "dropdown-menu"}, 
                                React.createElement("li", null, 
                                    React.createElement("a", {href: "#", 
                                       onClick: () => this.props.changePage({name: "performance/rules"})}, "Rules")
                                ), 
                                React.createElement("li", null, 
                                    React.createElement("a", {href: "#", onClick: () => this.props.changePage({name: "performance/predicates"})}, "Predicates")
                                ), 
                                React.createElement("li", null, 
                                    React.createElement("a", {href: "#", onClick: () => this.props.changePage({name: "performance/indexes"})}, "Indexes")
                                )
                            )
                        )
                    ), 

                    React.createElement("ul", {className: "nav navbar-nav"}, 
                        React.createElement("li", {className: "dropdown"}, 
                            React.createElement("a", {href: "#", className: "dropdown-toggle", "data-toggle": "dropdown"}, 
                                "Compiler", 
                                React.createElement("className", {className: "caret"})
                            ), 
                            React.createElement("ul", {className: "dropdown-menu"}, 
                                React.createElement("li", null, 
                                    React.createElement("a", {href: "#", 
                                       onClick: () => this.props.changePage({name: "compiler/phases"})}, "Phases")
                                )
                            )
                        )
                    ), 

                    React.createElement("ul", {className: "nav navbar-nav navbar-right"}, 
                        React.createElement("li", null, 
                            React.createElement("a", {href: "#"}, " ", React.createElement("span", {className: "glyphicon glyphicon-refresh"}), " Refresh")
                        ), 

                        React.createElement(StatusIcon, {status: Status})
                    )
                )
            )
        );
    }
});

/**
 * PageHead component.
 */
var PageHead = React.createClass({displayName: "PageHead",
    propTypes: {
        name: React.PropTypes.string.isRequired
    },
    render: function () {
        return (
            React.createElement("div", {className: "page-header"}, 
                React.createElement("h1", null, this.props.name)
            )
        );
    }
});

/**
 * Landing page.
 */
var LandingPage = React.createClass({displayName: "LandingPage",
    propTypes: {
        relations: React.PropTypes.array.isRequired,
        lattices: React.PropTypes.array.isRequired
    },

    render: function () {
        var zero = Snapshots[0].time;
        var labels = Snapshots.map(s => (s.time - zero) / 1000);
        var facts = Snapshots.map(s => s.facts);
        var memory = Snapshots.map(s => s.memory);

        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Welcome to the Flix Debugger!"}), 

                React.createElement("div", {className: "row"}, 
                    React.createElement("div", {className: "col-xs-6"}, 
                        React.createElement("h4", null, "Total Facts"), 
                        React.createElement(LineChart, {width: 600, height: 250, labels: labels, data: facts}), 

                        React.createElement("h4", null, "Total Memory Usage"), 
                        React.createElement(LineChart, {width: 600, height: 250, labels: labels, data: memory})
                    ), 

                    React.createElement("div", {className: "col-xs-6"}, 
                        React.createElement("h3", null, "Relations"), 

                        React.createElement("div", {className: "list-group"}, 
                            this.props.relations.map(relation => {
                                return (
                                    React.createElement("a", {href: "#", className: "list-group-item"}, 
                                        relation.name, " ", React.createElement("span", {className: "badge"}, relation.size)
                                    )
                                );
                            })
                        ), 

                        React.createElement("h3", null, "Lattices"), 

                        React.createElement("div", {className: "list-group"}, 
                            this.props.lattices.map(lattice => {
                                return (
                                    React.createElement("a", {href: "#", className: "list-group-item"}, 
                                        lattice.name, " ", React.createElement("span", {className: "badge"}, lattice.size)
                                    )
                                );
                            })
                        )
                    )
                )
            )
        );
    }
});

/**
 * Status component.
 */
var StatusIcon = React.createClass({displayName: "StatusIcon",
    propTypes: {
        status: React.PropTypes.string.isRequired
    },

    render: function () {
        var status = this.props.status;

        if (status === "inprogress") {
            return (
                React.createElement("li", {className: "bg-info"}, 
                    React.createElement("a", {href: "#"}, 
                        React.createElement("span", {className: "glyphicon glyphicon-time"}), " ", React.createElement("strong", null, "Running")
                    )
                ))
        } else if (status === "completed") {
            return (
                React.createElement("li", {className: "bg-success"}, 
                    React.createElement("a", {href: "#"}, 
                        React.createElement("span", {className: "glyphicon glyphicon-ok-circle"}), " ", React.createElement("strong", null, "Completed")
                    )
                ))
        } else if (status === "crashed") {
            return (
                React.createElement("li", {className: "bg-danger"}, 
                    React.createElement("a", {href: "#"}, 
                        React.createElement("span", {className: "glyphicon glyphicon-warning-sign"}), " ", React.createElement("strong", null, "Crashed")
                    )
                ))
        } else {
            return (
                React.createElement("li", {className: "bg-warning"}, 
                    React.createElement("a", {href: "#"}, 
                        React.createElement("span", {className: "glyphicon glyphicon-question-sign"}), " ", React.createElement("strong", null, "Connection Lost")
                    )
                ))
        }
    }
});

/**
 * Relation page.
 */
var RelationPage = React.createClass({displayName: "RelationPage",
    propTypes: {
        name: React.PropTypes.string.isRequired,
        table: React.PropTypes.object.isRequired
    },
    render: function () {
        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: this.props.name}), 
                React.createElement(Table, {table: this.props.table})
            )
        );
    }
});

/**
 * Rules page.
 */
var RulesPage = React.createClass({displayName: "RulesPage",
    render: function () {
        var table = {
            cols: ["Location", "Rule", "Hits", "Total Time (msec)", "Query Time (msec/op)", "Throughput (ops/msec)"],
            rows: Rules.map(row => [
                    row["location"],
                    row["rule"],
                    numeral(row["hitcount"]).format('0,0'),
                    numeral(row["time"]).format('0,0') + " msec",
                    numeral(row["time"] / row["hitcount"]).format('0.0000') + " msec/op",
                    numeral(row["hitcount"] / row["time"]).format('0,0') + " ops/msec"
                ]
            ),
            align: ["left", "left", "right", "right", "right"]
        };

        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Performance / Rules"}), 

                React.createElement("div", {className: "panel panel-default"}, 
                    React.createElement("div", {className: "panel-body"}, 
                        "The table below shows the time consumed by each rule."
                    )
                ), 

                React.createElement(Table, {table: table})
            )
        );
    }
});

/**
 * Predicates page.
 */
var PredicatesPage = React.createClass({displayName: "PredicatesPage",
    render: function () {
        var table = {
            cols: ["Name", "Size", "Indexed Lookups", "Indexed Scans", "Full Scans"],
            align: ["left", "right", "right", "right", "right"],
            rows: Predicates.map(row => [
                    row["name"],
                    numeral(row["size"]).format('0,0'),
                    numeral(row["indexedLookups"]).format('0,0'),
                    numeral(row["indexedScans"]).format('0,0'),
                    numeral(row["fullScans"]).format('0,0')
                ]
            )
        };

        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Performance / Predicates"}), 

                React.createElement("div", {className: "panel panel-default"}, 
                    React.createElement("div", {className: "panel-body"}, 
                        "The table below shows the time consumed by lookups for each predicate."
                    )
                ), 

                React.createElement(Table, {table: table})
            )
        );
    }
});

/**
 * Indexes page.
 */
var IndexesPage = React.createClass({displayName: "IndexesPage",
    render: function () {
        var table = {
            cols: ["Collection", "Index", "Index Hits"],
            align: ["left", "left", "right"],
            rows: Indexes.map(row =>
                    [row["collection"], row["index"], numeral(row["hits"]).format('0,0')]
            )
        };

        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Performance / Indexes"}), 

                React.createElement("div", {className: "panel panel-default"}, 
                    React.createElement("div", {className: "panel-body"}, 
                        "The table below shows the usage of indexes of each relation and lattice."
                    )
                ), 

                React.createElement(Table, {table: table})
            )
        );
    }
});

/**
 * Phases page.
 */
var PhasesPage = React.createClass({displayName: "PhasesPage",
    propTypes: {
        notifyConnectionError: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
        return {phases: []};
    },

    componentDidMount: function () {
        this.tick();
    },

    tick: function () {
        $.ajax({
            method: "GET", dataType: 'json', url: URL + '/compiler/phases', success: function (data) {
                this.setState({phases: data});
            }.bind(this),
            error: this.props.notifyConnectionError
        });
    },

    render: function () {
        var labels = this.state.phases.map(o => o.name);
        var data = this.state.phases.map(o => o.time);

        // NB: The use of key = Math.random() is necessary to ensure that the component
        // is always recreated whenever the data is changes.

        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Compiler / Phases"}), 

                React.createElement("div", {className: "panel panel-default"}, 
                    React.createElement("div", {className: "panel-body"}, 
                        "The graph below shows the amount of time spent in various phases of the compiler." + ' ' +
                        "The time is reported in miliseconds."
                    )
                ), 

                React.createElement(BarChart, {key: Math.random(), width: 600, height: 400, labels: labels, data: data})
            )
        );
    }
});

/**
 * Table component.
 */
var Table = React.createClass({displayName: "Table",
    propTypes: {
        table: React.PropTypes.shape({
            cols: React.PropTypes.array.isRequired,
            rows: React.PropTypes.array.isRequired,
            align: React.PropTypes.arrayOf(React.PropTypes.string).isRequired
        })
    },
    render: function () {
        return (
            React.createElement("table", {className: "table table-striped table-condense table-hover"}, 
                React.createElement(TableHeader, {table: this.props.table}), 
                React.createElement(TableBody, {table: this.props.table})
            )
        );
    }
});

/**
 * Table Header component.
 */
var TableHeader = React.createClass({displayName: "TableHeader",
    render: function () {
        return (
            React.createElement("thead", null, 
            React.createElement("tr", null, 
                this.props.table.cols.map(function (col, idx) {
                    var className = getAlignment(this.props.table.align[idx]);
                    return React.createElement("th", {className: className}, col)
                }.bind(this))
            )
            )
        );
    }
});

/**
 * Table Body component.
 */
var TableBody = React.createClass({displayName: "TableBody",
    render: function () {
        return (
            React.createElement("tbody", null, 
            this.props.table.rows.map(function (row) {
                return React.createElement(TableRow, {align: this.props.table.align, row: row})
            }.bind(this))
            )
        );
    }
});

/**
 * Table Row component.
 */
var TableRow = React.createClass({displayName: "TableRow",
    render: function () {
        return (
            React.createElement("tr", null, 
                this.props.row.map(function (elm, idx) {
                    var className = getAlignment(this.props.align[idx]);
                    return React.createElement("td", {className: className}, elm)
                }.bind(this))
            )
        );
    }
});

/**
 * Returns the CSS alignment string corresponding to the given alignment.
 */
function getAlignment(text) {
    if (text === "left") {
        return "text-left";
    } else if (text === "center") {
        return "text-center";
    } else {
        return "text-right";
    }
}

/**
 * Chart.js global options.
 */
Chart.defaults.global.animation = false;
Chart.defaults.global.responsive = false;


/**
 * A BarChart component based on Chart.js
 */
var BarChart = React.createClass({displayName: "BarChart",
    propTypes: {
        labels: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
        data: React.PropTypes.arrayOf(React.PropTypes.number).isRequired,
        width: React.PropTypes.number.isRequired,
        height: React.PropTypes.number.isRequired
    },

    componentDidMount: function () {
        var barChartData = {
            labels: this.props.labels,
            datasets: [
                {
                    fillColor: "rgba(151,187,205,0.5)",
                    strokeColor: "rgba(151,187,205,0.8)",
                    highlightFill: "rgba(151,187,205,0.75)",
                    highlightStroke: "rgba(151,187,205,1)",
                    data: this.props.data
                }
            ]
        };

        var ctx = this.canvas.getContext("2d");
        this.chart = new Chart(ctx).Bar(barChartData);
    },

    componentWillUnmount: function () {
        console.log("Unmounting")
        this.chart.destroy();
    },

    render: function () {
        return React.createElement("canvas", {width: this.props.width, height: this.props.height, ref: (ref) => this.canvas = ref})
    }
});


/**
 * A LineChart component based on Chart.js
 */
var LineChart = React.createClass({displayName: "LineChart",
    propTypes: {
        labels: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
        data: React.PropTypes.arrayOf(React.PropTypes.number).isRequired,
        width: React.PropTypes.number.isRequired,
        height: React.PropTypes.number.isRequired
    },

    componentDidMount: function () {
        var lineChartData = {
            labels: this.props.labels,
            datasets: [
                {
                    fillColor: "rgba(151,187,205,0.2)",
                    strokeColor: "rgba(151,187,205,1)",
                    pointColor: "rgba(151,187,205,1)",
                    pointStrokeColor: "#fff",
                    pointHighlightFill: "#fff",
                    pointHighlightStroke: "rgba(151,187,205,1)",
                    data: this.props.data
                }
            ]
        };

        var ctx = this.canvas.getContext("2d");
        this.chart = new Chart(ctx).Line(lineChartData);
    },

    componentWillUnmount: function () {
        this.chart.destroy();
    },

    render: function () {
        return React.createElement("canvas", {width: this.props.width, height: this.props.height, ref: (ref) => this.canvas = ref})
    }
});

/**
 * Render app when the page is ready.
 */
$(document).ready(function () {
    var root = document.getElementById("container");
    ReactDOM.render(React.createElement(App, null), root);
});
