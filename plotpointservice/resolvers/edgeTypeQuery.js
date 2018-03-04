import {edge_type} from '../data'

export default function (obj, args, context, graphql) {

  return edge_type(obj.id)
}