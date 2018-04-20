import axios from 'axios';
import {PageHeader, Pagination} from 'bootstrap-react-components';
import React from 'react';
import {withRouter} from 'react-router';
import PlotPointList from '../components/PlotPointList';
import {checkHttpStatus, convertErrorToString, parseJSON} from '../utils';

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
		axios.get('/api/plotPoints?size=10&sort=name,asc')
				.then(checkHttpStatus)
				.then(parseJSON)
				.then(data => this.setState({
					plotPoints: data._embedded.plotPoints,
					page      : data.page,
					links     : data._links
				}))
				.catch(error => console.log('error: ', convertErrorToString(error)));
	}

	navigationButton(name, enabled, onClick, active) {
		let classNames = 'page-item ' + (enabled ? '' : 'disabled') + (active ? ' active' : '');
		return <li className={classNames}>
			<a className={'page-link'} href={'#'} tabIndex={-1} onClick={onClick}>{name}</a>
		</li>;
	}

	navigateToNewPlotPoint() {
		this.props.history.push('/newPlotPoint');
	}

	nextButton() {
		return this.navigationButton("Next", this.state.links.next, this.onNext);
	}

	onNext(e) {
		if (this.state.links.next) {
			axios.get(this.state.links.next.href)
					.then(checkHttpStatus)
					.then(parseJSON)
					.then(data => this.setState({
						plotPoints: data._embedded.plotPoints,
						page      : data.page,
						links     : data._links
					}))
					.catch(error => console.log('error: ', convertErrorToString(error)));
		}
	}

	onPage(pageNumber) {
		let dis =this;
		return function (e) {
			axios.get(`/api/plotPoints?size=10&sort=name,asc&page=${pageNumber}`)
					.then(checkHttpStatus)
					.then(parseJSON)
					.then(data => dis.setState({
						plotPoints: data._embedded.plotPoints,
						page      : data.page,
						links     : data._links
					}))
					.catch(error => console.log('error: ', convertErrorToString(error)));
		};
	}

	onPrevious(e) {
		if (this.state.links.prev) {
			axios.get(this.state.links.prev.href)
					.then(checkHttpStatus)
					.then(parseJSON)
					.then(data => this.setState({
						plotPoints: data._embedded.plotPoints,
						page      : data.page,
						links     : data._links
					}))
					.catch(error => console.log('error: ', convertErrorToString(error)));
		}
	}

	pageButtons() {
		let buttons = [];

		for (let i = 0; i < this.state.page.totalPages || 0; i++) {
			let onClick     = this.onPage(i);
			let currentPage = this.state.page.number;// === 0 ? 1 : this.state.page.number;
			buttons.push(this.navigationButton((i+1).toString(), this.state.page.totalPages, onClick, currentPage === i));
		}
		return buttons;
	}

	previousButton() {
		return this.navigationButton("Previous", this.state.links.prev, this.onPrevious);
	}


	plotPointList() {
		let pageInfo = this.state.page ? <span>{this.state.page.number + 1} / {this.state.page.totalPages}</span> : <span/>;
		return <div>
			<PlotPointList id={'mainPlotPointList'} plotPoints={this.state.plotPoints} page={this.state.page}
			               links={this.state.links}/>
			<ul className="pagination">
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
					{this.state.plotPoints.length > 0 ? this.plotPointList() : <p>There are no plot points, please add one</p>}
				</div>
		);
	}
}

export default withRouter(PlotPoints);
