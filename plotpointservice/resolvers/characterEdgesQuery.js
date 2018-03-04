import {character_edges} from '../data'

export default function (obj, args, context, graphql) {

  return character_edges(obj.id)
}