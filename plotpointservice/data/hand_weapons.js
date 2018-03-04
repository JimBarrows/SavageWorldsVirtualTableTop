import database from '../database'

export function hand_weapons_for_plot_point (plot_point_id) {
  return database.query("select id, name, description, weight, cost, damage from hand_weapons where plot_point_id = $1 order by name", plot_point_id)
}

export function hand_weapon_category (handWeaponId) {
  return database.one(`select gear_category.id as id, gear_category.name as name, gear_category.description as description
                            from gear_category, hand_weapons
                            where hand_weapons.id = $1
                            and hand_weapons.gear_category_id = gear_category.id`, handWeaponId)
}

export function hand_weapon_notes (handWeaponId) {
  return database.query(`select gear_notes.id as id,
			gear_notes.name as name,
			gear_notes.description as description
 	    from hand_weapons, hand_weapon_gear_note, gear_notes
 	    where hand_weapon_gear_note.hand_weapon_id = $1
	    and hand_weapon_gear_note.gear_notes_id = gear_notes.id
 	    order by name`, handWeaponId)
}


export function hand_weapon_technology_level (handWeaponId) {
  return database.one(`select technology_levels.id as id,
      technology_levels.name as name,
      technology_levels.description as description
    from hand_weapons, technology_levels
    where hand_weapons.id = $1
    and hand_weapons.technology_level_id = technology_levels.id
    order by name`, handWeaponId)
}
