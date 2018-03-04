import database from '../database'

export function mundane_items_for_plot_point (plotPointId) {
  return database.query("select id, name, description, weight, cost from mundane_items where plot_point_id = $1 order by name", plotPointId)
}

export function mundane_item_category (mundaneItemId) {
  return database.one(`select gear_category.id as id, gear_category.name as name, gear_category.description as description
																				from gear_category, mundane_items
																				where mundane_items.id = $1
																				and mundane_items.gear_category_id = gear_category.id`, mundaneItemId)
}

export function mundane_item_notes (mundaneItemId) {
  return database.query(`select gear_notes.id as id,
			gear_notes.name as name,
			gear_notes.description as description
 	    from mundane_items, mundane_item_gear_note, gear_notes
 	    where mundane_item_gear_note.mundane_item_id = $1
	    and mundane_item_gear_note.gear_notes_id = gear_notes.id
 	    order by name`, mundaneItemId)
}


export function mundane_item_technology_level (mundaneItemId) {
  return database.one(`select technology_levels.id as id,
      technology_levels.name as name,
      technology_levels.description as description
    from mundane_items, technology_levels
    where mundane_items.id = $1
    and mundane_items.technology_level_id = technology_levels.id
    order by name`, mundaneItemId)
}
