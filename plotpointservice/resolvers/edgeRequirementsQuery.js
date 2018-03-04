import {edge_requirements} from '../data'

export default function (obj, args, context, graphql) {

  return edge_requirements(obj.id)
}