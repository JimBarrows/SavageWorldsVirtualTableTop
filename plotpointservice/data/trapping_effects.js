import database from '../database'

export function trappings_effects_for_trapping (trapping_id) {
  return database.query(`select id, name, description from trapping_effects where trapping_id = $1`, [trapping_id])
}
