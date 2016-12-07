import React from "react";
import AbilityEditor from "./Editor";
import AbilityView from "./View";
import {ItemDescription} from "../ItemList";


class AbilityDescription extends ItemDescription {

	editor(item) {
		return <AbilityEditor _id={item._id}
		                      name={item.name}
		                      description={item.description}
		                      cost={item.cost}
		                      save={this.save.bind(this)}
		                      onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <AbilityView _id={item._id}
		                    name={item.name}
		                    description={item.description}
		                    cost={item.cost}
		                    edit={this.editing.bind(this)}
		                    remove={this.remove.bind(this)}
		                    allowEditing={this.state.allowEditing}/>;
	}

}

export default AbilityDescription;