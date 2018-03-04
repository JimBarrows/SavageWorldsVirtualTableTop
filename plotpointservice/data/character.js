import database from '../database'

export function characters (plotpointId) {
  return database.query(`select
                            id,
                            agility,
                            armor,
                            charisma,
                            description,
                            experience_points,
                            name,
                            pace,
                            parry,
                            power_points,
                            rank,
                            smarts,
                            spirit,
                            strength,
                            toughness,
                            vigor,
                            wild_card
                        from characters
                        where plot_point_id = $1
                        order by name
                      `, plotpointId)
}

export function character_ammunition (characterId) {
  return database.query(`select
                            ammunition.id,
                            ammunition.name,
                            ammunition.description,
                            ammunition.weight,
                            ammunition.cost,
                            character_ammunition.amount as amount
                        from ammunition, character_ammunition
                        where character_ammunition.character_id = $1
                          and character_ammunition.ammunition_id = ammunition.id
                          `, characterId)
}

export function character_edges (characterId) {
  return database.query(`select
                          edges.id as id,
                          edges.name as name,
                          edges.description as description,
                          edges.effects as effects
                      from edges, character_edge
                      where character_edge.character_id = $1
                        and character_edge.edge_id = edges.id
                        `, characterId)
}

export function character_hand_weapons (characterId) {
  return database.query(`select
                          hand_weapons.id as id,
                          hand_weapons.name as name,
                          hand_weapons.description as description,
                          hand_weapons.weight as weight,
                          hand_weapons.cost as cost,
                          hand_weapons.damage as damage,
                          hand_weapons.weight as weight
                      from hand_weapons, character_hand_weapon
                      where character_hand_weapon.character_id = $1
                        and character_hand_weapon.hand_weapon_id = hand_weapons.id
                        `, characterId)
}

export function character_hindrances (characterId) {
  return database.query(`select
                            hindrances.id as id,
                            hindrances.name as name,
                            hindrances.description as description,
                            hindrances.type as type,
                            character_hindrance.taken_as as taken_as
                        from hindrances, character_hindrance
                        where character_hindrance.character_id = $1
                          and character_hindrance.hindrance_id = hindrances.id
                          `, characterId)
}

export function character_mundane_items (characterId) {
  return database.query(`select
                          mundane_items.id as id,
                          mundane_items.name as name,
                          mundane_items.description as description,
                          mundane_items.weight as weight,
                          mundane_items.cost as cost,
                          mundane_items.weight as weight
                      from mundane_items, character_mundane_item
                      where character_mundane_item.character_id = $1
                        and character_mundane_item.mundane_item_id = mundane_items.id
                        `, characterId)
}

export function character_powers (characterId) {
  return database.query(`select
                          id,
                          name,
                          power_id,
                          trapping_id,
                          notes
                      from character_power
                      where character_id = $1
                        `, characterId)
}

