import React from 'react';
import SettingRuleDescription from './Description';
import SettingRuleEditor from './Editor';
import ListManager from '../Item/List';

class SettingRuleList extends ListManager {

	render() {
		let {list, allowEditing}    = this.state;
		let addButton = this.buttonEditOrNothing('Add Setting', <SettingRuleEditor save={this.addToList.bind(this)}/>);

		return (
				<div id='settingRulesList'>
					<h1>Setting Rules</h1>
					{addButton}
					<dl>
						{list.map((item, index) => <SettingRuleDescription
								key={ item._id}
								item={item}
								save={this.updateItem.bind(this)}
								remove={this.removeItem.bind(this)}
								allowEditing={allowEditing}/>
						)}
					</dl>
				</div>
		);
	}


}

export default SettingRuleList;

