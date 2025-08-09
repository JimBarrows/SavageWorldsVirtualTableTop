import React, { Component } from 'react'
import * as PropTypes from 'prop-types'
import { Link } from 'react-router-dom'
import Scene from '../../propTypes/Scene'

export default class SceneList extends Component {
	static propTypes = {
		id: PropTypes.string.isRequired,
		scenes: PropTypes.arrayOf(Scene),
		onDelete: PropTypes.func
	}

	static defaultProps = {
		scenes: []
	}

	handleDelete = (sceneId) => {
		if (this.props.onDelete) {
			this.props.onDelete(sceneId)
		}
	}

	formatCharacterCount = (count) => {
		return count === 1 ? '1 character' : `${count} characters`
	}

	render() {
		return (
			<div data-testid={this.props.id}>
				<table className="table" role="table">
					<thead>
						<tr>
							<th>Name</th>
							<th>Description</th>
							<th>Characters</th>
							<th>Actions</th>
						</tr>
					</thead>
					<tbody>
						{this.props.scenes.map((scene, index) => (
							<tr key={scene.id || index}>
								<td>
									{scene.name}
									<Link to={`/scene/${encodeURIComponent(scene.name)}/edit`}>
										Edit
									</Link>
								</td>
								<td>{scene.description}</td>
								<td>
									{this.formatCharacterCount(scene.dramatis_personae?.length || 0)}
								</td>
								<td>
									<button
										type="button"
										className="btn btn-danger btn-sm"
										onClick={() => this.handleDelete(scene.id)}
									>
										Delete
									</button>
								</td>
							</tr>
						))}
					</tbody>
				</table>
			</div>
		)
	}
}