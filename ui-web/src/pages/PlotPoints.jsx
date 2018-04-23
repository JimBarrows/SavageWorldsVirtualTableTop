import {PageHeader} from 'bootstrap-react-components';
import React from 'react';
import {withRouter} from 'react-router';
import PlotPointList from '../components/PlotPointList';
import {connect} from "react-redux";
import {push} from "react-router-redux";
import {loadPage, loadNextPage, loadPreviousPage} from "../actions/PlotPointListActions";

class PlotPoints extends React.Component {

	constructor(props) {
		super(props);
		this.navigateToNewPlotPoint = this.navigateToNewPlotPoint.bind(this);
		this.navigationButton       = this.navigationButton.bind(this);
		this.nextButton             = this.nextButton.bind(this);
		this.onNext                 = this.onNext.bind(this);
		this.onPage                 = this.onPage.bind(this);
		this.onPrevious             = this.onPrevious.bind(this);
		this.pageButtons            = this.pageButtons.bind(this);
		this.previousButton         = this.previousButton.bind(this);
		this.plotPointList          = this.plotPointList.bind(this);

		this.state = {
			plotPoints: [],
			page      : {},
			links     : {}
		};
	}

	componentDidMount() {
		this.props.loadPage(0);
	}

	navigationButton(key, name, enabled, onClick, active) {
		let k          = key || '';
		let classNames = 'page-item ' + (enabled ? '' : 'disabled') + (active ? ' active' : '');
		return <li key={k} className={classNames}>
			<a className={'page-link'} tabIndex={-1} onClick={onClick}>{name}</a>
		</li>;
	}

	navigateToNewPlotPoint() {
		this.props.history.push('/plotPointEditor');
	}

	nextButton() {
		return this.navigationButton(this.props.page.totalPages, "Next", this.props.links.next, this.onNext);
	}

	onNext(e) {
		if (this.props.links.next) {
			this.props.loadNextPage(this.props.links.next.href);
		}
	}

	onPage(pageNumber) {
		let dis = this;
		return function (e) {
			dis.props.loadPage(pageNumber);
		};
	}

	onPrevious(e) {
		if (this.props.links.prev) {
			this.props.loadPrevPage();
		}
	}

	pageButtons() {
		let buttons = [];

		for (let i = 0; i < this.props.page.totalPages || 0; i++) {
			let onClick     = this.onPage(i);
			let currentPage = this.props.page.number;// === 0 ? 1 : this.props.page.number;
			buttons.push(this.navigationButton(i + 2, (i + 1).toString(), this.props.page.totalPages, onClick, currentPage === i));
		}
		return buttons;
	}

	previousButton() {
		return this.navigationButton(1, "Previous", this.props.links.prev, this.onPrevious);
	}


	plotPointList() {
		return <div>
			<PlotPointList id={'mainPlotPointList'} plotPoints={this.props.plotPoints} page={this.props.page}
			               links={this.props.links}/>
			<ul className="pagination justify-content-center">
				{this.previousButton()}
				{this.pageButtons()}
				{this.nextButton()}
			</ul>
		</div>;
	}

	render() {

		return (
				<div id='PlotPointsPage'>
					<PageHeader id='PlotPointsPage'><h1>Plot Points</h1></PageHeader>
					<button className={'btn btn-default'} id='addPlotPointButton' type={'button'}
					        onClick={this.navigateToNewPlotPoint}>Add
					</button>
					{this.props.plotPoints.length > 0 ? this.plotPointList() : <p>There are no plot points, please add one</p>}
				</div>
		);
	}
}

const mapStateToProps = (state) => {
	return {
		plotPoints: state.PlotPoints.plotPoints,
		page      : state.PlotPoints.page,
		links     : state.PlotPoints.links
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		create      : () => dispatch(push("/plotPoints/add")),
		loadPage        : (pageNumber) => dispatch(loadPage(pageNumber)),
		loadNextPage: () => dispatch(loadNextPage()),
		loadPrevPage: () => dispatch(loadPreviousPage())
	};
};

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(PlotPoints));
