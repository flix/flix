/**
 * Build instructions:
 *
 * Run the following command to install all the build dependencies:
 *
 *  $ npm install --save-dev react react-dom react-string-replace babel babel-preset-es2015 babel-preset-react babelify browserify uglify-js
 *
 * Then run:
 *
 *  $ browserify -t babelify app.jsx -o app.js
 *
 * To compile the JSX file.
 */
const React = require('react');
const ReactDOM = require('react-dom');
const reactStringReplace = require('react-string-replace')

/**
 * Main entry point.
 *
 * Renders the left navigation bar and the page content.
 */
export let App = React.createClass({
    render: function () {
        return (
            <div className="app">
                <LeftNavigationBar namespaces={this.props.namespaces}/>
                <PageContent namespace={this.props.data.namespace} data={this.props.data}/>
            </div>
        );
    }
});

/**
 * Renders the navigation bar which shows the list of namespaces in the library.
 */
export let LeftNavigationBar = React.createClass({
    render: function () {
        // Retrieve and sort the namespaces.
        const namespaces = this.props.namespaces.sort(lexicographic);

        // Construct a list item for each namespace in the library.
        const menu = namespaces.map(
            namespace => <li key={namespace.name}><a href={namespace.link}>{namespace.name}</a></li>
        );
        return (
            <div className="navbar">
                <div className="title">
                    <a href="index.html">
                        Flix Standard Library
                    </a>
                </div>
                <ul>{menu}</ul>
            </div>
        );
    }
});

/**
 * Renders the types, definitions, relations, and lattices declared in the current namespace.
 */
export let PageContent = React.createClass({
    render: function () {
        return (
            <div className="pagecontent">
                <h1>{this.props.namespace}</h1>
                <TypeList decls={this.props.data.types}/>
                <DefinitionList decls={this.props.data.definitions}/>
                <LawList decls={this.props.data.laws}/>
                <TestList decls={this.props.data.tests}/>
                <RelationList decls={this.props.data.relations}/>
                <LatticeList decls={this.props.data.lattices}/>
            </div>
        );
    }
});

/**
 * Renders a list of type declarations.
 */
export let TypeList = React.createClass({
    render: function () {
        const typeList = this.props.decls.map(d => <TypeBox key={d.name} decl={d}/>);
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
export let TypeBox = React.createClass({
    render: function () {
        const name = this.props.decl.name;
        const comment = format(this.props.decl.comment);
        return (
            <div id={name} className="type-decl">
                <div className="signature">
                    <span className="keyword">enum</span>
                    <span className="name">{name}</span>
                    <span className="anchor">
                        <a href={'#' + name}><i className="material-icons md-18">link</i></a>
                    </span>
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
export let DefinitionList = React.createClass({
    render: function () {
        const decls = this.props.decls.sort(lexicographic);
        const definitionList = decls.map(d => <DefinitionBox key={d.name} decl={d}/>);
        if (definitionList.length == 0)
            return null;
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
export let DefinitionBox = React.createClass({
    render: function () {
        const name = this.props.decl.name;
        const tparams = surround(intersperse(this.props.decl.tparams.map(
            tparam => <span key={Math.random()}>{tparam.name}</span>
        ), ", "), "[", "]");
        const fparams = surround(intersperse(this.props.decl.fparams.map(
            fparam => <span key={Math.random()}>{fparam.name}: <span key={Math.random()}
                                                                     className="type">{fparam.tpe}</span></span>
        ), ", "), "(", ")");
        const result = this.props.decl.result;
        const comment = format(this.props.decl.comment);
        return (
            <div id={name} className="definition-decl">
                <div className="signature">
                    <span className="keyword">def</span>
                    <span className="name">{name}</span>
                    <span className="tparams">{tparams}</span>
                    <span className="fparams">{fparams}</span>
                    <span className="result">: <span className="type">{result}</span></span>
                    <span className="anchor">
                        <a href={'#' + name}><i className="material-icons md-18">link</i></a>
                    </span>
                </div>
                <div className="comment">
                    {comment}
                </div>
            </div>
        );
    }
});

/**
 * Renders a list of law definitions.
 */
export let LawList = React.createClass({
    render: function () {
        const decls = this.props.decls.sort(lexicographic);
        const definitionList = decls.map(d => <DefinitionBox key={d.name} decl={d}/>);
        if (definitionList.length == 0)
            return null;
        return (
            <div className="definition-list">
                <h2>Laws</h2>
                {definitionList}
            </div>
        );
    }
});

/**
 * Renders a list of test definitions.
 */
export let TestList = React.createClass({
    render: function () {
        const decls = this.props.decls.sort(lexicographic);
        const definitionList = decls.map(d => <DefinitionBox key={d.name} decl={d}/>);
        if (definitionList.length == 0)
            return null;
        return (
            <div className="definition-list">
                <h2>Tests</h2>
                {definitionList}
            </div>
        );
    }
});

/**
 * Renders a list of relation declarations.
 */
export let RelationList = React.createClass({
    render: function () {
        const decls = this.props.decls.sort(lexicographic);
        const relationList = decls.map(d => <RelationBox key={d.name} decl={d}/>);
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
export let RelationBox = React.createClass({
    render: function () {
        const name = this.props.decl.name;
        const attributes = surround(intersperse(this.props.decl.attributes.map(
            attr => <span key={Math.random()}>{attr.name}: <span key={Math.random()} className="type">{attr.tpe}</span></span>
        ), ", "), "(", ")");
        const comment = format(this.props.decl.comment);
        return (
            <div id={name} className="relation-decl">
                <div className="signature">
                    <span className="keyword">rel</span>
                    <span className="name">{name}</span>
                    <span className="attributes">{attributes}</span>
                    <span className="anchor">
                        <a href={'#' + name}><i className="material-icons md-18">link</i></a>
                    </span>
                </div>
                <div className="comment">{comment}</div>
            </div>
        );
    }
});

/**
 * Renders a list of lattice declarations.
 */
export let LatticeList = React.createClass({
    render: function () {
        const decls = this.props.decls.sort(lexicographic);
        const latticeList = decls.map(d => <LatticeBox key={d.name} decl={d}/>);
        if (latticeList.length == 0)
            return null;
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
export let LatticeBox = React.createClass({
    render: function () {
        const name = this.props.decl.name;
        const attributes = surround(intersperse(this.props.decl.attributes.map(
            attr => <span key={Math.random()}>{attr.name}: <span key={Math.random()} className="type">{attr.tpe}</span></span>
        ), ", "), "(", ")");
        const comment = format(this.props.decl.comment);
        return (
            <div id={name} className="lattice-decl">
                <div className="signature">
                    <span className="keyword">rel</span>
                    <span className="name">{name}</span>
                    <span className="attributes">{attributes}</span>
                    <span className="anchor">
                        <a href={'#' + name}><i className="material-icons md-18">link</i></a>
                    </span>
                </div>
                <div className="comment">{comment}</div>
            </div>
        );
    }
});

/**
 * Formats the given documentation comment.
 */
function format(s) {
    return reactStringReplace(s, /`([^`]+)`/g, (txt, id) =>
        <span id={id} className="code">{txt}</span>
    );
}

/**
 * Returns the given array of elements surrounded by parenthesis (if non-empty).
 */
function surround(arr, b, e) {
    if (arr.length == 0)
        return arr;

    const result = [];
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

    return arr.slice(1).reduce(function (xs, x) {
        return xs.concat([sep, x]);
    }, [arr[0]]);
}

/**
 * Lexicographically compares the name field of the two given objects.
 */
function lexicographic(a, b) {
    const x = a.name;
    const y = b.name;
    return naturalSort(x, y);
}

/**
 * Natural Sort algorithm for Javascript - Version 0.7 - Released under MIT license
 * Author: Jim Palmer (based on chunking idea from Dave Koelle)
 */
function naturalSort(a, b, options) {
    var re = /(^-?[0-9]+(\.?[0-9]*)[df]?e?[0-9]?$|^0x[0-9a-f]+$|[0-9]+)/gi,
        sre = /(^[ ]*|[ ]*$)/g,
        dre = /(^([\w ]+,?[\w ]+)?[\w ]+,?[\w ]+\d+:\d+(:\d+)?[\w ]?|^\d{1,4}[\/\-]\d{1,4}[\/\-]\d{1,4}|^\w+, \w+ \d+, \d{4})/,
        hre = /^0x[0-9a-f]+$/i,
        ore = /^0/,
        options = options || {},
        i = function (s) {
            return options.insensitive && ('' + s).toLowerCase() || '' + s
        },
        // convert all to strings strip whitespace
        x = i(a).replace(sre, '') || '',
        y = i(b).replace(sre, '') || '',
        // chunk/tokenize
        xN = x.replace(re, '\0$1\0').replace(/\0$/, '').replace(/^\0/, '').split('\0'),
        yN = y.replace(re, '\0$1\0').replace(/\0$/, '').replace(/^\0/, '').split('\0'),
        // numeric, hex or date detection
        xD = parseInt(x.match(hre)) || (xN.length !== 1 && x.match(dre) && Date.parse(x)),
        yD = parseInt(y.match(hre)) || xD && y.match(dre) && Date.parse(y) || null,
        oFxNcL, oFyNcL,
        mult = options.desc ? -1 : 1;
    // first try and sort Hex codes or Dates
    if (yD)
        if (xD < yD) return -1 * mult;
        else if (xD > yD) return 1 * mult;
    // natural sorting through split numeric strings and default strings
    for (var cLoc = 0, numS = Math.max(xN.length, yN.length); cLoc < numS; cLoc++) {
        // find floats not starting with '0', string or 0 if not defined (Clint Priest)
        oFxNcL = !(xN[cLoc] || '').match(ore) && parseFloat(xN[cLoc]) || xN[cLoc] || 0;
        oFyNcL = !(yN[cLoc] || '').match(ore) && parseFloat(yN[cLoc]) || yN[cLoc] || 0;
        // handle numeric vs string comparison - number < string - (Kyle Adams)
        if (isNaN(oFxNcL) !== isNaN(oFyNcL)) {
            return (isNaN(oFxNcL) ? 1 : -1) * mult;
        }
        // rely on string comparison if different types - i.e. '02' < 2 != '02' < '2'
        else if (typeof oFxNcL !== typeof oFyNcL) {
            oFxNcL += '';
            oFyNcL += '';
        }
        if (oFxNcL < oFyNcL) return -1 * mult;
        if (oFxNcL > oFyNcL) return 1 * mult;
    }
    return 0;
}

/**
 * Boot the application.
 */
function bootstrap() {
    // Render application.
    ReactDOM.render(<App namespaces={window.menu} data={window.page}/>, document.getElementById("app"));

    // Trigger jump to anchor (if any).
    if (window.location.hash) {
        const id = window.location.hash.substr(1);
        if (id) {
            const elm = document.getElementById(id);
            if (elm) {
                elm.scrollIntoView({behavior: "smooth"});
            }
        }
    }
}

// Export the bootstrap function to the global window object.
window.bootstrap = bootstrap;
