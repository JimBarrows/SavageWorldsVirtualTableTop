package org.savageworlds.admin.rest;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.ws.rs.Path;

import org.savageworlds.admin.dto.EdgeTypeList;
import org.savageworlds.game.model.EdgeType;
import org.savageworlds.repository.EdgeTypeRepository;

@RequestScoped
@Path("/edgeTypes")
public class EdgeTypeEndpoint extends EndpointTemplate<EdgeType, EdgeTypeRepository, EdgeTypeList> {

	@EJB
	private EdgeTypeRepository repo;
	
	@Override
	protected EdgeTypeRepository repo() {
		return repo;
	}

	@Override
	public EdgeTypeList listAll(Integer startPosition, Integer maxResult) {
		return new EdgeTypeList( repo.findAll());
	}

}
