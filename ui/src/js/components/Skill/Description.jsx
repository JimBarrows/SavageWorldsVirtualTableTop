import React from "react";
import SkillEditor from "./Editor";
import SkillView from "./View";
import {ItemDescription} from "../Item";

class SkillDescription extends ItemDescription {

	editor(item) {
		return <SkillEditor _id={item._id}
		                    name={item.name}
		                    description={item.description}
		                    attribute={item.attribute}
		                    save={this.save.bind(this)}
		                    onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <SkillView _id={item._id}
		                  name={item.name}
		                  description={item.description}
		                  attribute={item.attribute}
		                  edit={this.editing.bind(this)}
		                  remove={this.remove.bind(this)}
		                  allowEditing={this.state.allowEditing}/>;
	}

}

export default SkillDescription;