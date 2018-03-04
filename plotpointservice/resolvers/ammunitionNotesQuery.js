import {ammunition_notes} from '../data'

export default function (obj, args, context, graphql) {

  return ammunition_notes(obj.id)
}