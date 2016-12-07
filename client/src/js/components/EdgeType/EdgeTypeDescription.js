import React from "react";
import EdgeTypeEditor from "./EdgeTypeEditor";
import EdgeTypeView from "./EdgeTypeView";
import ItemDescription from "../ItemList/ItemDescription";

class EdgeTypeDescription extends ItemDescription {

	editor(item) {
		return <EdgeTypeEditor _id={item._id}
		                       name={item.name}
		                       description={item.description}
		                       save={this.save.bind(this)}
		                       onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <EdgeTypeView _id={item._id}
		                     name={item.name}
		                     description={item.description}
		                     edit={this.editing.bind(this)}
		                     remove={this.remove.bind(this)}
		                     allowEditing={this.state.allowEditing}/>
	}
}

export default EdgeTypeDescription;