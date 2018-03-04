import {armor_notes} from '../data'

export default function (obj, args, context, graphql) {
  return armor_notes(obj.id)
}