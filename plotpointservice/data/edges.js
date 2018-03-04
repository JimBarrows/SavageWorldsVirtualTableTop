import database from '../database'

export function edges_for_plot_point (plot_point_id) {
  return database.query(`select edges.id as id,
                          edges.name as name,
                          edges.description as description,
                          edges.effects as effects
                        from edges
                        where edges.plot_point_id = $1
                        order by name`, plot_point_id)
}

export function edge_requirements (edgeId) {
  return database.query(`select edge_requirements.id as id, edge_requirements.requirement as requirement
  from edges, edge_requirements, edge_edge_requirement
  where edges.id = $1
  and edge_edge_requirement.edge_id = edges.id
  and edge_edge_requirement.edge_requirement_id = edge_requirements.id `, edgeId)
}

export function edge_type (edgeId) {
  return database.one(`select edge_types.id as id, edge_types.name as name, edge_types.description as description
                                      from edges, edge_types
                                      where edges.id = $1
                                        and edges.edge_type_id = edge_types.id
                                      order by name`, edgeId)
}
