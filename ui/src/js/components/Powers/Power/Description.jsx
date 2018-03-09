import React from "react";
import PowerEditor from "./Editor";
import PowerView from "./View";
import {ItemDescription} from "../../Item";

class PowerDescription extends ItemDescription {

	editor(item) {
		return <PowerEditor _id={item._id}
		                    name={item.name}
		                    description={item.description}
		                    duration={item.duration}
		                    level={item.level}
		                    maintenance={item.maintenance}
		                    powerPoints={item.powerPoints}
		                    range={item.range}
		                    trappings={item.trappings}
		                    save={this.save.bind(this)}
		                    onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <PowerView _id={item._id}
		                  name={item.name}
		                  description={item.description}
		                  duration={item.duration}
		                  level={item.level}
		                  maintenance={item.maintenance}
		                  powerPoints={item.powerPoints}
		                  range={item.range}
		                  trappings={item.trappings}
		                  edit={this.editing.bind(this)}
		                  remove={this.remove.bind(this)}
		                  allowEditing={this.state.allowEditing}/>
	}

}

export default PowerDescription;