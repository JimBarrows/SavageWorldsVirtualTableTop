import React from "react";
import SettingRuleEditor from "./Editor";
import SettingRuleViewer from "./Viewer";
import {ItemDescription} from "../ItemList";

class SettingRuleDescription extends ItemDescription {

	editor(item) {
		return <SettingRuleEditor _id={item._id}
		                          name={item.name}
		                          descripiton={item.description}
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