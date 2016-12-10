import React from "react";
import TrappingEditor from "./Editor";
import TrappingView from "./View";
import {ItemDescription} from "../../Item";

class TrappingDescription extends ItemDescription {

	editor(item) {
		return <TrappingEditor _id={item._id}
		                       name={item.name}
		                       description={item.description}
		                       type={item.type}
		                       save={this.save.bind(this)}
		                       onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <TrappingView _id={item._id}
		                     name={item.name}
		                     description={item.description}
		                     type={item.type}
		                     edit={this.editing.bind(this)}
		                     remove={this.remove.bind(this)}
		                     allowEditing={this.state.allowEditing}/>
	}

}

export default TrappingDescription;