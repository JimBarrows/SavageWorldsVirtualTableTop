import React from "react";
import HindranceEditor from "./Editor";
import HindranceView from "./View";
import {ItemDescription} from "../Item";

class HindranceDescription extends ItemDescription {

	editor(item) {
		return <HindranceEditor _id={item._id}
		                        name={item.name}
		                        description={item.description}
		                        severity={item.severity}
		                        save={this.save.bind(this)}
		                        onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <HindranceView _id={item._id}
		                      name={item.name}
		                      description={item.description}
		                      severity={item.severity}
		                      edit={this.editing.bind(this)}
		                      remove={this.remove.bind(this)}
		                      allowEditing={this.state.allowEditing}/>
	}
}

export default HindranceDescription;