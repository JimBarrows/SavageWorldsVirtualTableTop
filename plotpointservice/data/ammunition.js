import database from '../database'

export class Ammunition {
	constructor(id, name, description, weight, cost) {
		this.id          = id;
		this.name        = name;
		this.description = description;
		this.weight      = weight;
		this.cost        = cost;
	}
}

export function ammunition_for_plot_point(plot_point_id) {
	return database.query("select id, name, description, weight, cost from ammunition where plot_point_id =$1", [plot_point_id])
			.then(ammunition_list => ammunition_list.map(ammo => new Ammunition(ammo.id, ammo.name, ammo.description, ammo.weight, ammo.cost)));
}

export function ammunition_by_id(id) {
	return database.query("select id, name, description, weight, cost from ammunition where id = $1 order by name", id)
			.then(ammo => new Ammunition(ammo.id, ammo.name, ammo.description, ammo.weight, ammo.cost));
}

export function ammunition_category (ammunition_id) {
  return database.one(`select gear_category.id as id, gear_category.name as name, gear_category.description as description
																				from gear_category, ammunition
																				where ammunition.id = $1
																				and ammunition.gear_category_id = gear_category.id`, ammunition_id)
}

export function ammunition_notes (ammunition_id) {
  return database.query(`select gear_notes.id as id,
      gear_notes.name as name,
      gear_notes.description as description
    from ammunition, ammunition_gear_note, gear_notes
    where ammunition_gear_note.ammunition_id = $1
    and ammunition_gear_note.gear_note_id = gear_notes.id
    order by name`, ammunition_id)
}
