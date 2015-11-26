/**
 * The URL to the REST API server.
 */
var URL = "http://" + window.location.hostname + ":" + window.location.port;

/**
 * A collection of common functionality.
 */
var Common = {};

/**
 * Triggers an AJAX request to the given URL.
 */
Common.ajax = function (url, failure, success) {
    $.ajax({method: "GET", dataType: 'json', url: url, success: success, error: failure});
};

/**
 * Main Application.
 */
var App = React.createClass({

    /**
     * An array of currently registered refreshable components.
     */
    refreshable: [],

    /**
     * The initial state of the application consists of:
     *
     * - A page object with the name of the page (and optionally the relation/lattice).
     * - A status string representing the status of the debugger/solver.
     * - An array of relation names (used in the menu and on the landing page).
     * - An array of lattices names (used in the menu and on the landing page).
     */
    getInitialState: function () {
        return {
            page: {name: "default"},
            status: "running",
            relations: [],
            lattices: []
        };
    },

    /**
     * Retrieve the list of relations and lattices when the component is mounted.
     */
    componentDidMount: function () {
        this.refresh();
    },

    /**
     * Refreshes the relations and lattices.
     */
    refresh: function () {
        // retrieve relations
        Common.ajax(URL + '/relations', this.notifyConnectionError, data => {
            this.setState({relations: data})
        });

        // retrieve lattices
        Common.ajax(URL + '/lattices', this.notifyConnectionError, data => {
            this.setState({lattices: data})
        });

        // retrieve status
        Common.ajax(URL + '/status', this.notifyConnectionError, data => {
            this.setState({status: data.status})
        });
    },

    /**
     * Triggers a page change.
     */
    changePage: function (page) {
        this.setState({page: page});
    },

    /**
     * Triggers a page refresh.
     */
    refreshPage: function () {
        // refresh this component
        this.refresh();

        // loop through each refreshable component and call it.
        this.refreshable.forEach(f => f())
    },

    /**
     * Registers a refreshable component.
     */
    registerRefreshCallback: function (fn) {
        this.refreshable.push(fn);
    },

    /**
     * Un-registers a refreshable component.
     */
    unregisterRefreshCallback: function (fn) {
        this.refreshable = this.refreshable.filter(x => x != fn);
    },

    /**
     * Notifies the application component that an AJAX communication has occurred.
     */
    notifyConnectionError: function () {
        // update the status to indicate that an error has occurred.
        this.setState({status: "connectionError"});
    },

    /**
     * Renders the application.
     */
    render: function () {
        // the name of the page.
        var pageName = this.state.page.name;

        // the page component to render (defaults to the landing page).
        var page = <LandingPage relations={this.state.relations}
                                lattices={this.state.lattices}
                                changePage={this.changePage}
                                registerRefreshCallback={this.registerRefreshCallback}
                                unregisterRefreshCallback={this.unregisterRefreshCallback}
                                notifyConnectionError={this.notifyConnectionError}/>;

        // select the page component based on the page name.
        if (pageName === "relation") {
            page = <RelationPage key={this.state.page.relation}
                                 name={this.state.page.relation}
                                 registerRefreshCallback={this.registerRefreshCallback}
                                 unregisterRefreshCallback={this.unregisterRefreshCallback}
                                 notifyConnectionError={this.notifyConnectionError}/>;
        }
        if (pageName === "lattice") {
            page = <LatticePage key={this.state.page.lattice}
                                name={this.state.page.lattice}
                                registerRefreshCallback={this.registerRefreshCallback}
                                unregisterRefreshCallback={this.unregisterRefreshCallback}
                                notifyConnectionError={this.notifyConnectionError}/>;
        }
        if (pageName === "performance/rules") {
            page = <RulesPage registerRefreshCallback={this.registerRefreshCallback}
                              unregisterRefreshCallback={this.unregisterRefreshCallback}
                              notifyConnectionError={this.notifyConnectionError}/>;
        }
        if (pageName === "performance/predicates") {
            page = <PredicatesPage registerRefreshCallback={this.registerRefreshCallback}
                                   unregisterRefreshCallback={this.unregisterRefreshCallback}
                                   notifyConnectionError={this.notifyConnectionError}/>;
        }
        if (pageName === "performance/indexes") {
            page = <IndexesPage registerRefreshCallback={this.registerRefreshCallback}
                                unregisterRefreshCallback={this.unregisterRefreshCallback}
                                notifyConnectionError={this.notifyConnectionError}/>;
        }
        if (pageName === "compiler/phases") {
            page = <PhasesPage registerRefreshCallback={this.registerRefreshCallback}
                               unregisterRefreshCallback={this.unregisterRefreshCallback}
                               notifyConnectionError={this.notifyConnectionError}/>;
        }

        return (
            <div>
                <Menu changePage={this.changePage}
                      refreshPage={this.refreshPage}
                      status={this.state.status}
                      relations={this.state.relations}
                      lattices={this.state.lattices}/>
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
        refreshPage: React.PropTypes.func.isRequired,
        relations: React.PropTypes.array.isRequired,
        lattices: React.PropTypes.array.isRequired,
        status: React.PropTypes.string.isRequired
    },

    /**
     * Renders the menu component.
     */
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
                                    return <li key={name}
                                               onClick={() => this.props.changePage({name: "relation", relation: name})}>
                                        <a href="#">{name}</a>
                                    </li>
                                })}

                                <li role="separator" className="divider"></li>

                                {this.props.lattices.map(lattice => {
                                    var name = lattice.name;
                                    return <li key={name}
                                               onClick={() => this.props.changePage({name: "lattice", lattice: name})}>
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
                                <li onClick={() => this.props.changePage({name: "performance/rules"})}>
                                    <a href="#">Rules</a>
                                </li>
                                <li onClick={() => this.props.changePage({name: "performance/predicates"})}>
                                    <a href="#">Predicates</a>
                                </li>
                                <li onClick={() => this.props.changePage({name: "performance/indexes"})}>
                                    <a href="#">Indexes</a>
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
                                <li onClick={() => this.props.changePage({name: "compiler/phases"})}>
                                    <a href="#"> Phases </a>
                                </li>
                            </ul>
                        </li>
                    </ul>

                    <ul className="nav navbar-nav navbar-right">
                        <li onClick={this.props.refreshPage}>
                            <a href="#">
                                <span className="glyphicon glyphicon-refresh"></span> Refresh
                            </a>
                        </li>

                        <StatusLine status={this.props.status}/>
                    </ul>
                </div>
            </nav>
        );
    }
});

/**
 * The status line component.
 */
var StatusLine = React.createClass({
    propTypes: {
        status: React.PropTypes.string.isRequired
    },

    /**
     * Renders the status line.
     */
    render: function () {
        var status = this.props.status;

        var text = null;
        var icon = null;
        var className = null;

        switch (status) {
            case "running":
                text = "Running";
                icon = "glyphicon glyphicon-time";
                className = "bg-info";
                break;
            case "complete":
                text = "Complete";
                icon = "glyphicon glyphicon-ok-circle";
                className = "bg-success";
                break;
            default:
                text = "No Connection";
                icon = "glyphicon glyphicon-question-sign";
                className = "bg-danger"
        }

        return (
            <li className={className}>
                <a href="#">
                    <span className={icon}></span> <strong>{text}</strong>
                </a>
            </li>)
    }
});


/**
 * The landing page component.
 */
var LandingPage = React.createClass({
    propTypes: {
        relations: React.PropTypes.array.isRequired,
        lattices: React.PropTypes.array.isRequired,
        changePage: React.PropTypes.func.isRequired,
        registerRefreshCallback: React.PropTypes.func.isRequired,
        unregisterRefreshCallback: React.PropTypes.func.isRequired,
        notifyConnectionError: React.PropTypes.func.isRequired
    },

    /**
     * The state of this component consists of an array of snapshots.
     */
    getInitialState: function () {
        return {snapshots: []};
    },

    /**
     * Refresh the AJAX data on mount. Register the component as refreshable.
     */
    componentDidMount: function () {
        this.refresh();
        this.props.registerRefreshCallback(this.refresh);
    },

    /**
     * Un-register the component as refreshable.
     */
    componentWillUnmount: function () {
        this.props.unregisterRefreshCallback(this.refresh);
    },

    /**
     * Retrieves JSON data from the server.
     */
    refresh: function () {
        Common.ajax(URL + '/snapshots/', this.notifyConnectionError, data => {
            this.setState({snapshots: data});
        });
    },

    /**
     * Renders the landing page.
     */
    render: function () {
        if (this.state.snapshots.length == 0) {
            return <div>Loading ...</div>
        }

        var labels = this.state.snapshots.map(s => Math.round(s.time / 1000));
        var queue = this.state.snapshots.map(s => s.queue);
        var facts = this.state.snapshots.map(s => s.facts);
        var memory = this.state.snapshots.map(s => s.memory);

        var last = this.state.snapshots[this.state.snapshots.length - 1];
        var currentQueuelength = numeral(last.queue).format('0,0');
        var currentNumberOfacts = numeral(last.facts).format('0,0');
        var currentMemoryUsage = numeral(last.memory).format('0,0');

        return (
            <div>
                <div className="page-header">
                    <h1>Welcome to the Flix Debugger</h1>
                </div>

                <div className="row">

                    <div className="col-xs-6">
                        <h3>Worklist ({currentQueuelength} items)</h3>
                        <LineChart key={Math.random()} width={600} height={250} labels={labels} data={queue}
                                   theme="blue"/>

                        <h3>Database ({currentNumberOfacts} facts)</h3>
                        <LineChart key={Math.random()} width={600} height={250} labels={labels} data={facts}
                                   theme="magenta"/>

                        <h3>Memory Usage ({currentMemoryUsage} MB)</h3>
                        <LineChart key={Math.random()} width={600} height={250} labels={labels} data={memory}
                                   theme="orangered"/>
                    </div>

                    <div className="col-xs-6">
                        <h3>Relations</h3>

                        <div className="list-group">
                            {this.props.relations.map(relation => {
                                return (
                                    <a href="#" className="list-group-item"
                                       onClick={() => this.props.changePage({name: "relation", relation: relation.name})}>
                                        {relation.name}
                                        <span className="badge">{numeral(relation.size).format('0,0')}</span>
                                    </a>
                                );
                            })}
                        </div>

                        <h3>Lattices</h3>

                        <div className="list-group">
                            {this.props.lattices.map(lattice => {
                                return (
                                    <a href="#" className="list-group-item"
                                       onClick={() => this.props.changePage({name: "lattice", lattice: lattice.name})}>
                                        {lattice.name}
                                        <span className="badge">{numeral(lattice.size).format('0,0')}</span>
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
 * Relation page.
 */
var RelationPage = React.createClass({
    propTypes: {
        name: React.PropTypes.string.isRequired,
        registerRefreshCallback: React.PropTypes.func.isRequired,
        unregisterRefreshCallback: React.PropTypes.func.isRequired,
        notifyConnectionError: React.PropTypes.func.isRequired
    },

    /**
     * The state is an object which holds an array of columns (strings) and rows (arrays of strings).
     */
    getInitialState: function () {
        return {table: {cols: [], rows: []}};
    },

    /**
     * Refresh the AJAX data on mount. Register the component as refreshable.
     */
    componentDidMount: function () {
        this.refresh();
        this.props.registerRefreshCallback(this.refresh);
    },

    /**
     * Un-register the component as refreshable.
     */
    componentWillUnmount: function () {
        this.props.unregisterRefreshCallback(this.refresh);
    },

    /**
     * Retrieves JSON data from the server.
     */
    refresh: function () {
        Common.ajax(URL + '/relation/' + this.props.name, this.notifyConnectionError, data => {
            this.setState({table: data})
        });
    },

    /**
     * Renders this component.
     */
    render: function () {
        var numberOfFacts = numeral(this.state.table.rows.length).format('0,0');

        return (
            <div>
                <div className="page-header">
                    <h1>{"Relation / " + this.props.name} ({numberOfFacts})</h1>
                </div>

                <Table table={this.state.table}/>
            </div>
        );
    }
});


/**
 * Lattice page.
 */
var LatticePage = React.createClass({
    propTypes: {
        name: React.PropTypes.string.isRequired,
        registerRefreshCallback: React.PropTypes.func.isRequired,
        unregisterRefreshCallback: React.PropTypes.func.isRequired,
        notifyConnectionError: React.PropTypes.func.isRequired
    },

    /**
     * The state is an object which holds an array of columns (strings) and rows (arrays of strings).
     */
    getInitialState: function () {
        return {table: {cols: [], rows: []}};
    },

    /**
     * Refresh the AJAX data on mount. Register the component as refreshable.
     */
    componentDidMount: function () {
        this.refresh();
        this.props.registerRefreshCallback(this.refresh);
    },

    /**
     * Un-register the component as refreshable.
     */
    componentWillUnmount: function () {
        this.props.unregisterRefreshCallback(this.refresh);
    },

    /**
     * Retrieves JSON data from the server.
     */
    refresh: function () {
        Common.ajax(URL + '/lattice/' + this.props.name, this.notifyConnectionError, data => {
            this.setState({table: data})
        });
    },

    /**
     * Renders the component.
     */
    render: function () {
        var numberOfFacts = numeral(this.state.table.rows.length).format('0,0');

        return (
            <div>
                <div className="page-header">
                    <h1>{"Lattice / " + this.props.name} ({numberOfFacts})</h1>
                </div>

                <Table table={this.state.table}/>
            </div>
        );
    }
});

/**
 * Rules page.
 */
var RulesPage = React.createClass({
    propTypes: {
        registerRefreshCallback: React.PropTypes.func.isRequired,
        unregisterRefreshCallback: React.PropTypes.func.isRequired,
        notifyConnectionError: React.PropTypes.func.isRequired
    },

    /**
     * The state is an array of rules where a rule is a {location, rule, hits, time}-object.
     */
    getInitialState: function () {
        return {rules: []};
    },

    /**
     * Refresh the AJAX data on mount. Register the component as refreshable.
     */
    componentDidMount: function () {
        this.refresh();
        this.props.registerRefreshCallback(this.refresh);
    },

    /**
     * Un-register the component as refreshable.
     */
    componentWillUnmount: function () {
        this.props.unregisterRefreshCallback(this.refresh);
    },

    /**
     * Retrieves JSON data from the server.
     */
    refresh: function () {
        Common.ajax(URL + '/performance/rules', this.notifyConnectionError, data => {
            this.setState({rules: data})
        });
    },

    /**
     * Renders the component.
     */
    render: function () {
        var table = {
            cols: ["Location", "Rule", "Hits", "Total Time (msec)", "Latency (msec/op)", "Throughput (ops/msec)"],
            rows: this.state.rules.map(row => {
                    var location = row.loc;
                    var rule = row.rule;
                    var hits = numeral(row.hits).format('0,0');
                    var time = numeral(row.time).format('0,0') + " msec";
                    var latency = numeral(row.time / row.hits).format('0.0000') + " msec/op";
                    var throughput = numeral(row.hits / row.time).format('0,0') + " ops/msec";
                    return [location, rule, hits, time, latency, throughput];
                }
            ),
            align: ["left", "left", "right", "right", "right"]
        };

        return (
            <div>
                <div className="page-header">
                    <h1>Performance / Rules</h1>
                </div>

                <div className="panel panel-default">
                    <div className="panel-body">
                        The table shows the number of miliseconds spent in evaluation of each rule.
                    </div>
                </div>

                <Table table={table}/>
            </div>
        );
    }
});

/**
 * Predicates page.
 */
var PredicatesPage = React.createClass({
    propTypes: {
        registerRefreshCallback: React.PropTypes.func.isRequired,
        unregisterRefreshCallback: React.PropTypes.func.isRequired,
        notifyConnectionError: React.PropTypes.func.isRequired
    },

    /**
     * The state is an array of predicates where a predicate is a
     * {name, size, indexedLookups, indexedScans, fullScans} object.
     */
    getInitialState: function () {
        return {predicates: []};
    },

    /**
     * Refresh the AJAX data on mount. Register the component as refreshable.
     */
    componentDidMount: function () {
        this.refresh();
        this.props.registerRefreshCallback(this.refresh);
    },

    /**
     * Un-register the component as refreshable.
     */
    componentWillUnmount: function () {
        this.props.unregisterRefreshCallback(this.refresh);
    },

    /**
     * Retrieves JSON data from the server.
     */
    refresh: function () {
        Common.ajax(URL + '/performance/predicates', this.notifyConnectionError, data => {
            this.setState({predicates: data})
        });
    },

    /**
     * Renders the component.
     */
    render: function () {
        var table = {
            cols: ["Name", "Size", "Indexed Lookups", "Indexed Scans", "Full Scans"],
            align: ["left", "right", "right", "right", "right"],
            rows: this.state.predicates.map(row => [
                    row["name"],
                    numeral(row.size).format('0,0'),
                    numeral(row.indexedLookups).format('0,0'),
                    numeral(row.indexedScans).format('0,0'),
                    numeral(row.fullScans).format('0,0')
                ]
            )
        };

        return (
            <div>
                <div className="page-header">
                    <h1>Performance / Predicates</h1>
                </div>

                <div className="panel panel-default">
                    <div className="panel-body">
                        The table shows the number of miliseconds spent in evaluation of each predicate.
                    </div>
                </div>

                <Table table={table}/>
            </div>
        );
    }
});

/**
 * Indexes page.
 */
var IndexesPage = React.createClass({
    propTypes: {
        registerRefreshCallback: React.PropTypes.func.isRequired,
        unregisterRefreshCallback: React.PropTypes.func.isRequired,
        notifyConnectionError: React.PropTypes.func.isRequired
    },

    /**
     * The state is an array of indexes where an index is a {name, index, hits} object.
     * @returns {{indexes: Array}}
     */
    getInitialState: function () {
        return {indexes: []};
    },

    /**
     * Refresh the AJAX data on mount. Register the component as refreshable.
     */
    componentDidMount: function () {
        this.refresh();
        this.props.registerRefreshCallback(this.refresh);
    },

    /**
     * Un-register the component as refreshable.
     */
    componentWillUnmount: function () {
        this.props.unregisterRefreshCallback(this.refresh);
    },

    /**
     * Retrieves JSON data from the server.
     */
    refresh: function () {
        Common.ajax(URL + '/performance/indexes', this.notifyConnectionError, data => {
            this.setState({indexes: data})
        });
    },

    /**
     * Renders the component.
     */
    render: function () {
        var table = {
            cols: ["Name", "Index", "Index Hits"],
            align: ["left", "left", "right"],
            rows: this.state.indexes.map(row => [row.name, row.index, numeral(row["hits"]).format('0,0')])
        };

        return (
            <div>
                <div className="page-header">
                    <h1>Performance / Indexes</h1>
                </div>

                <div className="panel panel-default">
                    <div className="panel-body">
                        The table shows the usage of indexes for each relation and lattice.
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
    propTypes: {
        registerRefreshCallback: React.PropTypes.func.isRequired,
        unregisterRefreshCallback: React.PropTypes.func.isRequired,
        notifyConnectionError: React.PropTypes.func.isRequired
    },

    /**
     * The state is an array of phases where a phase is {name, time} object.
     */
    getInitialState: function () {
        return {phases: []};
    },

    /**
     * Refresh the AJAX data on mount. Register the component as refreshable.
     */
    componentDidMount: function () {
        this.refresh();
        this.props.registerRefreshCallback(this.refresh);
    },

    /**
     * Un-register the component as refreshable.
     */
    componentWillUnmount: function () {
        this.props.unregisterRefreshCallback(this.refresh);
    },

    /**
     * Retrieves JSON data from the server.
     */
    refresh: function () {
        Common.ajax(URL + '/compiler/phases', this.notifyConnectionError, data => {
            this.setState({phases: data})
        });
    },

    /**
     * Renders the component.
     */
    render: function () {
        var labels = this.state.phases.map(o => o.name);
        var time = this.state.phases.map(o => o.time);

        return (
            <div>
                <div className="page-header">
                    <h1>Compiler / Phases</h1>
                </div>

                <div className="panel panel-default">
                    <div className="panel-body">
                        The graph shows the number of miliseconds spent in each phase of the compiler.
                    </div>
                </div>

                <BarChart key={Math.random()} width={600} height={400} labels={labels} data={time}/>
            </div>
        );
    }
});

/**
 * A table component.
 */
var Table = React.createClass({
    propTypes: {
        table: React.PropTypes.shape({
            cols: React.PropTypes.array.isRequired,
            rows: React.PropTypes.array.isRequired
        })
    },
    render: function () {
        return (
            <table className="table table-condense table-hover">
                <TableHeader table={this.props.table}/>
                <TableBody table={this.props.table}/>
            </table>
        );
    }
});

/**
 * A table header component.
 */
var TableHeader = React.createClass({
    render: function () {
        return (
            <thead>
            <tr>
                {this.props.table.cols.map((col, idx) => <th
                    className={getAlignment(idx, this.props.table.align)}>{col}</th>)}
            </tr>
            </thead>
        );
    }
});

/**
 * A table body component.
 */
var TableBody = React.createClass({
    render: function () {
        return (
            <tbody className="monospaced">
            {this.props.table.rows.map(row => <TableRow align={this.props.table.align} row={row}/>)}
            </tbody>
        );
    }
});

/**
 * A table row component.
 */
var TableRow = React.createClass({
    render: function () {
        return (
            <tr>
                {this.props.row.map((elm, idx) =>  <td className={getAlignment(idx, this.props.align)}>{elm}</td>)}
            </tr>
        );
    }
});

/**
 * Returns the CSS alignment string corresponding to the given alignment.
 */
function getAlignment(idx, align) {
    if (align === undefined) {
        return "text-left";
    }

    var text = align[idx];
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
var BarChart = React.createClass({
    propTypes: {
        labels: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
        data: React.PropTypes.arrayOf(React.PropTypes.number).isRequired,
        width: React.PropTypes.number.isRequired,
        height: React.PropTypes.number.isRequired
    },

    /**
     * Draw the bar chart once the canvas element has been created.
     */
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

    /**
     * Release the resources associated with the bar chart.
     */
    componentWillUnmount: function () {
        this.chart.destroy();
    },

    /**
     * Render the bar chart.
     *
     * NB: The canvas element must be re-created before a bar chart can be redrawn.
     */
    render: function () {
        return <canvas width={this.props.width} height={this.props.height} ref={(ref) => this.canvas = ref}/>
    }
});


/**
 * A LineChart component based on Chart.js
 */
var LineChart = React.createClass({
    propTypes: {
        labels: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
        data: React.PropTypes.arrayOf(React.PropTypes.number).isRequired,
        width: React.PropTypes.number.isRequired,
        height: React.PropTypes.number.isRequired
    },

    /**
     * Draw the line chart once the canvas element has been created.
     */
    componentDidMount: function () {
        var dataset = LineChart.getBlueDataSet(this.props.data);

        if (this.props.theme === "magenta") {
            dataset = LineChart.getMagentaDataSet(this.props.data);
        }
        if (this.props.theme === "orangered") {
            dataset = LineChart.getOrangeRedDataSet(this.props.data);
        }

        var lineChartData = {
            labels: this.props.labels,
            datasets: [dataset]
        };

        var ctx = this.canvas.getContext("2d");
        this.chart = new Chart(ctx).Line(lineChartData);
    },

    /**
     * Release the resources associated with the line chart.
     */
    componentWillUnmount: function () {
        this.chart.destroy();
    },

    /**
     * Render the line chart.
     *
     * NB: The canvas element must be re-created before a line chart can be redrawn.
     */
    render: function () {
        return <canvas width={this.props.width} height={this.props.height} ref={(ref) => this.canvas = ref}/>
    }
});

/**
 * Returns a data set colored blue.
 */
LineChart.getBlueDataSet = function (data) {
    return {
        fillColor: "rgba(151,187,205,0.2)",
        strokeColor: "rgba(151,187,205,1)",
        pointColor: "rgba(151,187,205,1)",
        pointStrokeColor: "#fff",
        pointHighlightFill: "#fff",
        pointHighlightStroke: "rgba(151,187,205,1)",
        data: data
    }
};

/**
 * Returns a data set colored magenta.
 */
LineChart.getMagentaDataSet = function (data) {
    return {
        fillColor: "rgb(243, 234, 245)",
        strokeColor: "rgb(195, 151, 205)",
        pointColor: "rgb(195, 151, 205)",
        pointStrokeColor: "#fff",
        pointHighlightFill: "#fff",
        pointHighlightStroke: "rgb(243, 234, 245)",
        data: data
    }
};

/**
 * Returns a data set colored orangered.
 */
LineChart.getOrangeRedDataSet = function (data) {
    return {
        fillColor: "rgb(245, 234, 234)",
        strokeColor: "rgb(205, 152, 151)",
        pointColor: "rgb(205, 152, 151)",
        pointStrokeColor: "#fff",
        pointHighlightFill: "#fff",
        pointHighlightStroke: "rgb(245, 234, 234)",
        data: data
    }
};

/**
 * Mount the App component when the document has loaded.
 */
$(document).ready(function () {
    var root = document.getElementById("container");
    ReactDOM.render(React.createElement(App, null), root);
});

