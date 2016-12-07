import React from "react";
import SettingRuleDescription from "./Description";
import SettingRuleEditor from "./Editor";
import ListManager from "../ItemList/ItemList";

class SettingRuleList extends ListManager {


	render() {
		let {list, allowEditing}    = this.state;
		let addButton               = this.buttonEditOrNothing("Add Setting", <SettingRuleEditor
				save={this.addToList.bind(this)}/>);

		return (
				<div id="settingRulesList">
					<h2>Setting Rules</h2>
					{addButton}
					<dl>
						{list.map((item, index) => <SettingRuleDescription
								key={item._id}
								item={item}
								save={this.addToList.bind(this)}
								update={this.updateItem.bind(this)}
								remove={this.removeItem.bind(this)}
								allowEditing={allowEditing}/>
						)}
					</dl>
				</div>
		);
	}


}

export default SettingRuleList;

