import database from '../database'

export class Plot_Point {
  constructor (id, name, brief_description, description) {
    this.id = id
    this.name = name
    this.brief_description = brief_description
    this.description = description
  }

}

export function create_plot_point (brief_description, description, name) {
  return database.one("insert into plot_points(brief_description, description, name) values ($1, $2, $3) returning id", [brief_description, description, name])
}

export function delete_plot_point (id) {
  console.log("delete_plot_point: id: ", id)
  return database.none("delete from plot_points where id = $1", id)
  .then(() => new Plot_Point(id, "", "", ""))
}

export function plot_points () {
  return database.query('select id, name, brief_description, description from plot_points order by name')
  .then(plot_points => plot_points.map(pp => new Plot_Point(pp.id, pp.name, pp.brief_description, pp.description)))
}

export function plot_point_by_id (id) {
  return database.one('select id, name, brief_description, description from plot_points where id = $1', id)
  .then(pp => new Plot_Point(pp.id, pp.name, pp.brief_description, pp.description))
}

export function update_plot_point (brief_description, description, id, name) {
  return database.none('update plot_points set brief_description = $1, description = $2, name=$3 where id = $4', [brief_description, description, name, id])
  .then(pp => new Plot_Point(id, name, brief_description, description))
}

