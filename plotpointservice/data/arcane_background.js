import database from '../database'

export function arcane_backgrounds_for_plot_point (plotpointId) {
  return database.query(`select id ,
                                name,
                                description,
                                skill_id,
                                starting_power_points,
                                starting_powers
                            from arcane_backgrounds
                            where 
                              plot_point_id = $1
                            `, plotpointId)
}

export function arcane_background_skill (arcaneBackgroundId) {
  return database.one(`select id ,
                              name,
                              description,
                              ability
                          from skills
                          where 
                            id = $1
                          `, arcaneBackgroundId)
}