var PointsTo = {
    cols: ["localVal", "value"],
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
var App = React.createClass({

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
            page = <PhasesPage />
        } else if (pageName === "performance/indexusage") {
            page = <IndexUsagePage />
        } else if (pageName === "performance/queries") {
            page = <QueriesPage />
        } else if (pageName === "relation") {
            page = <RelationPage name="VarPointsTo" table={PointsTo}/>
        } else {
            page = <LandingPage relations={Relations} lattices={Lattices}/>
        }

        return (
            <div>
                <Menu changePage={this.changePage} status={Status} relations={Relations} lattices={Lattices}/>
                {page}
            </div>
        );
    }
});

/**
 * Menu component.
 */
var Menu = React.createClass({
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
            <nav className="navbar navbar-default">
                <div className="container-fluid">
                    <div className="navbar-header">
                        <a className="navbar-brand" href="#" onClick={() => this.props.changePage({name: "default"})}>
                            Flix Debugger
                        </a>
                    </div>

                    <ul className="nav navbar-nav">
                        <li className="dropdown">
                            <a href="#" className="dropdown-toggle" data-toggle="dropdown">
                                Minimal Model <span className="caret"></span>
                            </a>
                            <ul className="dropdown-menu">
                                {this.props.relations.map(relation => {
                                    var name = relation.name;
                                    return <li key={name} onClick={this.changePageRelation({name})}>
                                        <a href="#">{name}</a>
                                    </li>
                                })}

                                <li role="separator" className="divider"></li>

                                {this.props.lattices.map(lattice => {
                                    var name = lattice.name;
                                    return <li key={name}>
                                        <a href="#">{name}</a>
                                    </li>
                                })}
                            </ul>
                        </li>
                    </ul>

                    <ul className="nav navbar-nav">
                        <li className="dropdown">
                            <a href="#" className="dropdown-toggle" data-toggle="dropdown">
                                Performance
                                <className className="caret"></className>
                            </a>
                            <ul className="dropdown-menu">
                                <li>
                                    <a href="#" onClick={() => this.props.changePage({name: "performance/indexusage"})}>Index
                                        Usage</a>
                                </li>
                                <li>
                                    <a href="#" onClick={() => this.props.changePage({name: "performance/queries"})}>Queries</a>
                                </li>
                            </ul>
                        </li>
                    </ul>

                    <ul className="nav navbar-nav">
                        <li className="dropdown">
                            <a href="#" className="dropdown-toggle" data-toggle="dropdown">
                                Compiler
                                <className className="caret"></className>
                            </a>
                            <ul className="dropdown-menu">
                                <li>
                                    <a href="#" onClick={() => this.props.changePage({name: "performance/phases"})}>Phases</a>
                                </li>
                            </ul>
                        </li>
                    </ul>

                    <ul className="nav navbar-nav navbar-right">
                        <li>
                            <a href="#"> <span className="glyphicon glyphicon-refresh"></span> Refresh</a>
                        </li>

                        <StatusIcon status={Status}/>
                    </ul>
                </div>
            </nav>
        );
    }
});

/**
 * PageHead component.
 */
var PageHead = React.createClass({
    propTypes: {
        name: React.PropTypes.string.isRequired
    },
    render: function () {
        return (
            <div className="page-header">
                <h1>{this.props.name}</h1>
            </div>
        );
    }
});

/**
 * Landing page.
 */
var LandingPage = React.createClass({
    propTypes: {
        relations: React.PropTypes.array.isRequired,
        lattices: React.PropTypes.array.isRequired
    },

    render: function () {
        return (
            <div>
                <PageHead name="Welcome to the Flix Debugger!"/>

                <div className="row">
                    <div className="col-xs-6">
                        <h3>Relations</h3>

                        <div className="list-group">
                            {this.props.relations.map(relation => {
                                return (
                                    <a href="#" className="list-group-item">
                                        {relation.name} <span className="badge">{relation.size}</span>
                                    </a>
                                );
                            })}
                        </div>
                    </div>

                    <div className="col-xs-6">
                        <h3>Lattices</h3>


                        <div className="list-group">
                            {this.props.lattices.map(lattice => {
                                return (
                                    <a href="#" className="list-group-item">
                                        {lattice.name} <span className="badge">{lattice.size}</span>
                                    </a>
                                );
                            })}
                        </div>
                    </div>
                </div>
            </div>
        );
    }
});

/**
 * Status component.
 */
var StatusIcon = React.createClass({
    propTypes: {
        status: React.PropTypes.string.isRequired
    },

    render: function () {
        var status = this.props.status;

        if (status === "inprogress") {
            return (
                <li className="bg-info">
                    <a href="#">
                        <span className="glyphicon glyphicon-time"></span> <strong>Running</strong>
                    </a>
                </li>)
        } else if (status === "completed") {
            return (
                <li className="bg-success">
                    <a href="#">
                        <span className="glyphicon glyphicon-ok-circle"></span> <strong>Completed</strong>
                    </a>
                </li>)
        } else if (status === "crashed") {
            return (
                <li className="bg-danger">
                    <a href="#">
                        <span className="glyphicon glyphicon-warning-sign"></span> <strong>Crashed</strong>
                    </a>
                </li>)
        } else {
            return (
                <li className="bg-warning">
                    <a href="#">
                        <span className="glyphicon glyphicon-question-sign"></span> <strong>Connection Lost</strong>
                    </a>
                </li>)
        }
    }
});

/**
 * Relation page.
 */
var RelationPage = React.createClass({
    propTypes: {
        name: React.PropTypes.string.isRequired,
        table: React.PropTypes.object.isRequired
    },
    render: function () {
        return (
            <div>
                <PageHead name={this.props.name}/>
                <Table table={this.props.table}/>
            </div>
        );
    }
});

/**
 * A BarChart component based on Chart.js
 */
var BarChart = React.createClass({
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
        return <canvas width={this.props.width} height={this.props.height} ref={(ref) => this.canvas = ref}/>
    }
});

/**
 * IndexUsage page.
 */
var IndexUsagePage = React.createClass({
    render: function () {
        var table = {
            cols: ["Collection", "Index", "Index Hits"],
            align: ["left", "left", "right"],
            rows: Indexes.map(row =>
                    [row["collection"], row["index"], numeral(row["hits"]).format('0,0')]
            )
        };

        return (
            <div>
                <PageHead name="Performance / Index Usage"/>

                <div className="panel panel-default">
                    <div className="panel-body">
                        The table below shows the index usage of every collection.
                    </div>
                </div>

                <Table table={table}/>
            </div>
        );
    }
});

/**
 * Queries page.
 */
var QueriesPage = React.createClass({
    render: function () {
        var table = {
            cols: ["Location", "Rule", "Hits", "Total Time (msec)", "Query Time (msec/op)", "Throughput (ops/msec)"],
            rows: Queries.map(row => [
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
            <div>
                <PageHead name="Performance / Queries"/>

                <div className="panel panel-default">
                    <div className="panel-body">
                        The table below shows the most time consuming queries.
                    </div>
                </div>

                <Table table={table}/>
            </div>
        );
    }
});

/**
 * Phases page.
 */
var PhasesPage = React.createClass({
    render: function () {
        var labels = Phases.map(o => o["phase"]);
        var data = Phases.map(o => o["time"]);

        return (
            <div>
                <PageHead name="Compiler / Phases"/>

                <div className="panel panel-default">
                    <div className="panel-body">
                        The graph below shows the amount of time spent in various phases of the compiler.
                        The time is reported in miliseconds.
                    </div>
                </div>

                <BarChart width={600} height={400} labels={labels} data={data}/>
            </div>
        );
    }
});

/**
 * Table component.
 */
var Table = React.createClass({
    propTypes: {
        table: React.PropTypes.shape({
            cols: React.PropTypes.array.isRequired,
            rows: React.PropTypes.array.isRequired,
            align: React.PropTypes.arrayOf(React.PropTypes.string).isRequired
        })
    },
    render: function () {
        return (
            <table className="table table-striped table-condense table-hover">
                <TableHeader table={this.props.table}/>
                <TableBody table={this.props.table}/>
            </table>
        );
    }
});

/**
 * Table Header component.
 */
var TableHeader = React.createClass({
    render: function () {
        return (
            <thead>
            <tr>
                {this.props.table.cols.map(function (col, idx) {
                    var className = getAlignment(this.props.table.align[idx]);
                    return <th className={className}>{col}</th>
                }.bind(this))}
            </tr>
            </thead>
        );
    }
});

/**
 * Table Body component.
 */
var TableBody = React.createClass({
    render: function () {
        return (
            <tbody>
            {this.props.table.rows.map(function (row) {
                return <TableRow align={this.props.table.align} row={row}/>
            }.bind(this))}
            </tbody>
        );
    }
});

/**
 * Table Row component.
 */
var TableRow = React.createClass({
    render: function () {
        return (
            <tr>
                {this.props.row.map(function (elm, idx) {
                    var className = getAlignment(this.props.align[idx]);
                    return <td className={className}>{elm}</td>
                }.bind(this))}
            </tr>
        );
    }
});

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
 * Render app when the page is ready.
 */
$(document).ready(function () {
    var root = document.getElementById("container");
    ReactDOM.render(React.createElement(App, null), root);
});

