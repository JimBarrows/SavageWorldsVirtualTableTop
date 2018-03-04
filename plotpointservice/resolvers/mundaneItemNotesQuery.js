import {mundane_item_notes} from '../data'

export default function (obj, args, context, graphql) {
  return mundane_item_notes(obj.id)
}