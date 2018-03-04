import database from '../database'

export function hindrances_for_plot_point (plot_point_id) {
  return database.query(`select hindrances.id as id,
        hindrances.name as name,
        hindrances.description as description,
        hindrances.type as type
      from hindrances
      where hindrances.plot_point_id = $1
      order by name`, plot_point_id)
}
