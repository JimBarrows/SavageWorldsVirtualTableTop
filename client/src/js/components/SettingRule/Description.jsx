import React from "react";
import SettingRuleEditor from "./Editor";
import SettingRuleViewer from "./View";
import {ItemDescription} from "../Item";

class SettingRuleDescription extends ItemDescription {

	editor(item) {
		return <SettingRuleEditor _id={item._id}
		                          name={item.name}
		                          description={item.description}
		                          save={this.save.bind(this)}
		                          onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <SettingRuleViewer _id={item._id}
		                          name={item.name}
		                          description={item.description}
		                          edit={this.editing.bind(this)}
		                          remove={this.remove.bind(this)}
		                          allowEditing={this.state.allowEditing}/>
	}


}

export default SettingRuleDescription;