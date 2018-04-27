import React from 'react';
import RaceViewer from './View';
import RaceEditor from './Editor';


class RaceDescription  {

	editor(item) {
		return <RaceEditor _id={item._id}
		                   name={item.name}
		                   description={item.description}
		                   abilities={item.abilities}
		                   save={this.save.bind(this)}
		                   onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <RaceViewer _id={item._id}
		                   name={item.name}
		                   description={item.description}
		                   abilities={item.abilities}
		                   edit={this.editing.bind(this)}
		                   remove={this.remove.bind(this)}
		                   allowEditing={this.state.allowEditing}/>
	}

}

export default RaceDescription;