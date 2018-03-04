import database from '../database'

export function armor_for_plot_point (plotPointId) {
  return database.query("select id, name, description, weight, cost, armor from armors where plot_point_id = $1 order by name", plotPointId)
}

export function armor_category (armorId) {
  return database.one(`select gear_category.id as id, gear_category.name as name, gear_category.description as description
																				from gear_category, armors
																				where armors.id = $1
																				and armors.gear_category_id = gear_category.id`, armorId)
}

export function armor_notes (armorId) {
  return database.query(`select gear_notes.id as id,
			gear_notes.name as name,
			gear_notes.description as description
 	    from armors, armor_gear_note, gear_notes
 	    where armor_gear_note.armor_id = $1
	    and armor_gear_note.gear_notes_id = gear_notes.id
 	    order by name`, armorId)
}


export function armor_technology_level (armorId) {
  return database.one(`select technology_levels.id as id,
      technology_levels.name as name,
      technology_levels.description as description
    from armors, technology_levels
    where armors.id = $1
    and armors.technology_level_id = technology_levels.id
    order by name`, armorId)
}
