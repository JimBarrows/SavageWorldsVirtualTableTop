import database from '../database'

export function skills_for_plot_point (plot_point_id) {
  return database.query(`select skills.id as id,
                          skills.name as name,
                          skills.description as description,
                          skills.ability as ability
                        from skills
                        where skills.plot_point_id = $1
                        order by name`, plot_point_id)
}
