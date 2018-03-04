import database from '../database'

export function power_by_id (power_id) {
  return database.one(`select 
                            id, 
                            name, 
                            description, 
                            rank, 
                            plot_point_id,
                            power_points, 
                            range, 
                            duration, 
                            trappings 
                          from powers 
                          where id = $1`, [power_id])
}

export function powers_for_plot_point (plot_point_id) {
  return database.query(`select 
                            id, 
                            name, 
                            description, 
                            rank, 
                            plot_point_id,
                            power_points, 
                            range, 
                            duration, 
                            trappings 
                          from powers 
                          where plot_point_id = $1`, [plot_point_id])
}