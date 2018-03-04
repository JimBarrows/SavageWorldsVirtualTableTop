import database from '../database'

export function trappings_for_plot_point (plotPointId) {
  return database.query(`select id, plot_point_id, name, description from trappings where plot_point_id = $1`, [plotPointId])
}

export function trapping_by_id (trappingId) {
  return database.one(`select id, plot_point_id, name, description from trappings where id = $1`, [trappingId])
}
