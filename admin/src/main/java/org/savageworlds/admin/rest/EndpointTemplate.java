package org.savageworlds.admin.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;

import jdo.core.repository.RepositoryTemplate;
import jdo.model.BasePersistentModel;

public abstract class EndpointTemplate<E extends BasePersistentModel, R extends RepositoryTemplate<E, Long>, L> {

	abstract protected R  repo();
	
	/**
	 * @param armordescription
	 * @return
	 */
	@POST
	@Consumes("application/json")
	public E create( E entity) { 
		return repo().create( entity);
	}

	/**
	 * @param id
	 * @return
	 */
	@GET
	@Path("/{id:[0-9][0-9]*}")
	@Produces("application/json")
	public E findById(@PathParam("id") final Long id) {
		E entity= repo().findById( id);
		if( entity == null) {
			throw new NotFoundException();
		}
		return entity;
	}

	/**
	 * @param startPosition
	 * @param maxResult
	 * @return
	 */
	@GET
	@Produces("application/json")
	abstract public L listAll(@QueryParam("start") final Integer startPosition, @QueryParam("max") final Integer maxResult); 

	/**
	 * @param id
	 * @param armordescription
	 * @return
	 */
	@PUT
	@Path("/{id:[0-9][0-9]*}")
	@Consumes("application/json")
	public E update(@PathParam("id") Long id, final E entity) {
		if ((id == null) || (id < 0)) {
			throw new IllegalArgumentException("Id must be part of path, and greater than 0.");
		}
		entity.setId(id);
		return repo().update(entity);
	}

	/**
	 * @param id
	 * @return
	 */
	@DELETE
	@Path("/{id:[0-9][0-9]*}")
	public void deleteById(@PathParam("id") final Long id) {
		
		repo().delete( id);
	}
}
