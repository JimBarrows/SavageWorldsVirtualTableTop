import React from "react";
import SettingRule from "./SettingRule";
import ListManager from "./ItemList/ItemList";

class SettingRuleList extends ListManager {


	render() {
		let {list}       = this.state;
		let addButton    = this.buttonEditOrNothing("Add Setting", <SettingRule adding={true}
		                                                                        save={this.addToList.bind(this)}/>);

		return (
				<div id="settingRules">
					<h2>Setting Rules</h2>
					{addButton}
					<dl>
						{list.map((rule, index) => <SettingRule
								key={rule._id || `${rule.name.replace(" ", "_")}_${index}`}
								rule={rule}
								index={index}
								save={this.addToList.bind(this)}
								update={this.updateItem.bind(this)}
								remove={this.removeItem.bind(this)}/>
						)}
					</dl>
				</div>
		);
	}


}

export default SettingRuleList;

