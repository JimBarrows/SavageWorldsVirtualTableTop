import database from '../database'

export class SettingRule {
	constructor(id, name, description) {
		this.id          = id;
		this.name        = name;
		this.description = description;
	}
}

export function setting_rules_for_plot_point(plot_point_id) {
	return database.query("select id, name, description from setting_rules where plot_point_id = $1", [plot_point_id])
			.then(setting_rules => setting_rules.map(sr => new SettingRule(sr.id, sr.name, sr.description)));
}