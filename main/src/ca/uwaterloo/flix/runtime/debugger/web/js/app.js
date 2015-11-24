var PointsTo = {
    columns: ["localVal", "value"],
    rows: [
        [1, "/ParityAnalysis::Parity.Odd(())"],
        [2, "/ParityAnalysis::Parity.Even(())"],
        [3, "/ParityAnalysis::Parity.Odd(())"],
        [7, "/ParityAnalysis::Parity.Odd(())"],
        [8, "/ParityAnalysis::Parity.Top(())"]
    ]
};

var Phases = [
    {phase: "Parser", time: 243},
    {phase: "Weeder", time: 231},
    {phase: "Namer", time: 86},
    {phase: "Linker", time: 24},
    {phase: "Typer", time: 467},
    {phase: "Normalizer", time: 412},
    {phase: "Emitter", time: 357}
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

var Queries = [
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


var Relations = [
    {name: "Multi", size: 148},
    {name: "AddrOf", size: 915},
    {name: "KillEmpty", size: 0},
    {name: "PtSU", size: 0},
    {name: "PtH", size: 811},
    {name: "AllObjects", size: 915},
    {name: "FILoad", size: 8},
    {name: "Copy", size: 481},
    {name: "Store", size: 316},
    {name: "CFG", size: 4525},
    {name: "Pt", size: 14103},
    {name: "Clear", size: 225},
    {name: "FIStore", size: 69},
    {name: "Load", size: 2139},
    {name: "Phi", size: 2702}
];

var Lattices = [
    {name: "SUBefore", size: 609793},
    {name: "SUBefore", size: 605760},
    {name: "Kill", size: 1571}
];

var Status = "completed";


/**
 * Main Application entry point.
 */
var App = React.createClass({displayName: "App",

    getInitialState: function () {
        return {page: {name: "default"}};
    },

    changePage: function (page) {
        this.setState({page: page})
    },

    render: function () {
        var page = null;
        var pageName = this.state.page.name;
        if (pageName === "performance/phases") {
            page = React.createElement(PhasesPage, null)
        } else if (pageName === "performance/indexes") {
            page = React.createElement(IndexesPage, null)
        } else if (pageName === "performance/queries") {
            page = React.createElement(QueriesPage, null)
        } else if (pageName === "relation") {
            page = React.createElement(RelationPage, {name: "VarPointsTo", table: PointsTo})
        } else {
            page = React.createElement(LandingPage, {relations: Relations, lattices: Lattices})
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
                                    React.createElement("a", {href: "#", onClick: () => this.props.changePage({name: "performance/indexes"})}, "Indexes")
                                ), 
                                React.createElement("li", null, 
                                    React.createElement("a", {href: "#", onClick: () => this.props.changePage({name: "performance/queries"})}, "Queries")
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
                                    React.createElement("a", {href: "#", onClick: () => this.props.changePage({name: "performance/phases"})}, "Phases")
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
        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Welcome to the Flix Debugger!"}), 

                React.createElement("div", {className: "row"}, 
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
                        )
                    ), 

                    React.createElement("div", {className: "col-xs-6"}, 
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
        this.chart.destroy();
    },

    render: function () {
        return React.createElement("canvas", {width: this.props.width, height: this.props.height, ref: (ref) => this.canvas = ref})
    }
});

/**
 * Indexes page.
 */
var IndexesPage = React.createClass({displayName: "IndexesPage",
    render: function () {
        var table = {
            columns: ["Collection", "Index", "Hits"],
            rows: Indexes.map(row =>
                    [row["collection"], row["index"], row["hits"]]
            )
        };

        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Performance / Indexes"}), 
                React.createElement(Table, {table: table})
            )
        );
    }
});

/**
 * Queries page.
 */
var QueriesPage = React.createClass({displayName: "QueriesPage",
    render: function () {
        var table = {
            columns: ["Source Location", "Rule", "Hitcount", "Total Time (msec)", "Time / Operation (usec)"],
            rows: Queries.map(row =>
                    [row["location"], row["rule"], row["hitcount"], row["time"], 1000 * row["time"] / row["hitcount"]]
            )
        };

        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Performance / Queries"}), 
                React.createElement(Table, {table: table})
            )
        );
    }
});

/**
 * Phases page.
 */
var PhasesPage = React.createClass({displayName: "PhasesPage",
    render: function () {
        var labels = Phases.map(o => o["phase"]);
        var data = Phases.map(o => o["time"]);

        return (
            React.createElement("div", null, 
                React.createElement(PageHead, {name: "Performance / Phases"}), 

                React.createElement(BarChart, {width: 600, height: 400, labels: labels, data: data})
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
            columns: React.PropTypes.array.isRequired,
            rows: React.PropTypes.array.isRequired
        })
    },
    render: function () {
        return (
            React.createElement("table", {className: "table table-striped table-condense table-hover"}, 
                React.createElement(TableHeader, {columns: this.props.table.columns}), 
                React.createElement(TableBody, {rows: this.props.table.rows})
            )
        );
    }
});

/**
 * Table Header component.
 */
var TableHeader = React.createClass({displayName: "TableHeader",
    propTypes: {
        columns: React.PropTypes.array.isRequired
    },
    render: function () {
        return (
            React.createElement("thead", null, 
            React.createElement("tr", null, 
                this.props.columns.map(function (column) {
                    return React.createElement("th", {key: column}, column)
                })
            )
            )
        );
    }
});

/**
 * Table Body component.
 */
var TableBody = React.createClass({displayName: "TableBody",
    propTypes: {
        rows: React.PropTypes.array.isRequired
    },
    render: function () {
        return (
            React.createElement("tbody", null, 
            this.props.rows.map(function (row) {
                return React.createElement(TableRow, {key: row, row: row})
            })
            )
        );
    }
});

/**
 * Table Row component.
 */
var TableRow = React.createClass({displayName: "TableRow",
    propTypes: {
        row: React.PropTypes.array.isRequired
    },
    render: function () {
        return (
            React.createElement("tr", null, 
                this.props.row.map(function (elm) {
                    return React.createElement("td", null, elm)
                })
            )
        );
    }
});

/**
 * Chart.js global options.
 */
Chart.defaults.global.animation = false;
Chart.defaults.global.responsive = false;


/**
 * Render app when the page is ready.
 */
$(document).ready(function () {
    var root = document.getElementById("container");
    ReactDOM.render(React.createElement(App, null), root);
});
