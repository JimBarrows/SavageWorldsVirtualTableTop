import {characters} from '../data'

export default function (obj, args, context, graphql) {

  return characters(obj.id)
}