import React from "react";
import ArcaneBackgroundEditor from "./Editor";
import ArcaneBackgroundView from "./View";
import {ItemDescription} from "../../Item";

class ArcaneBackgroundDescription extends ItemDescription {

	editor(item) {
		return <ArcaneBackgroundEditor _id={item._id}
		                               name={item.name}
		                               description={item.description}
		                               skill={item.skill}
		                               startingPowerPoints={item.startingPowerPoints}
		                               startingPowers={item.startingPowers}
		                               save={this.save.bind(this)}
		                               onListChange={this.props.onListChange}/>
	}

	viewer(item) {
		return <ArcaneBackgroundView _id={item._id}
		                             name={item.name}
		                             description={item.description}
		                             skill={item.skill}
		                             startingPowerPoints={item.startingPowerPoints}
		                             startingPowers={item.startingPowers}
		                             edit={this.editing.bind(this)}
		                             remove={this.remove.bind(this)}
		                             allowEditing={this.state.allowEditing}/>
	}

}

export default ArcaneBackgroundDescription;