import {InMemoryCache} from 'apollo-cache-inmemory'
import ApolloClient from 'apollo-client'
import {HttpLink} from 'apollo-link-http'

import {setWorldConstructor} from 'cucumber'
import config from "./config"
import {swga_db, user_db} from "./database"
import expected_plot_point from './expected_plot_point'

class CustomWorld {
  constructor () {
    this.config = config
    this.expected = {}
    this.expected.plotPoints = expected_plot_point
    this.plot_point = {}
    this.plotPointService = new ApolloClient({
                                               link: new HttpLink({uri: config.plotPointServer.url}),
                                               cache: new InMemoryCache()
                                             })
    this.result = {
      error: {},
      data: {}
    }
    this.swga_db = swga_db
    this.user_db = user_db
    this.user = {}
    this.userService = new ApolloClient({
                                          link: new HttpLink({uri: config.userServer.url}),
                                          cache: new InMemoryCache()
                                        })
  }
}

setWorldConstructor(CustomWorld)
