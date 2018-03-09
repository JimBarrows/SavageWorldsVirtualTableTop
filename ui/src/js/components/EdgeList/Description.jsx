import React from "react";
import EdgeEditor from "./Editor";
import EdgeView from "./View";
import {ItemDescription} from "../Item";

class EdgeDescription extends ItemDescription {

	editor(item) {
		return <EdgeEditor _id={item._id}
		                   name={item.name}
		                   description={item.description}
		                   edgeType={item.edgeType}
		                   save={this.save.bind(this)}
		                   onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <EdgeView _id={item._id}
		                 name={item.name}
		                 description={item.description}
		                 edgeType={item.edgeType}
		                 edit={this.editing.bind(this)}
		                 remove={this.remove.bind(this)}
		                 allowEditing={this.state.allowEditing}/>
	}

}

export default EdgeDescription;