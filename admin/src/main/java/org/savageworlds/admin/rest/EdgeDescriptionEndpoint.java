package org.savageworlds.admin.rest;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
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

import org.savageworlds.admin.dto.EdgeDescriptionDto;
import org.savageworlds.admin.dto.EdgeDescriptionList;
import org.savageworlds.game.model.EdgeDescription;
import org.savageworlds.repository.EdgeDescriptionRepository;
import org.savageworlds.repository.EdgeTypeRepository;

@RequestScoped
@Path("/edgeDescriptions")
public class EdgeDescriptionEndpoint {

	@EJB
	private EdgeDescriptionRepository	repo;

	@EJB
	private EdgeTypeRepository				edgeTypeRepo;

	protected EdgeDescriptionRepository repo() {
		return repo;
	};

	/**
	 * @param dto
	 * @return
	 */
	@POST
	@Consumes("application/json")
	public EdgeDescriptionDto create(EdgeDescriptionDto dto) {

		return convertTo((repo().create(convertTo(dto))));
	}

	/**
	 * @param id
	 * @return
	 */
	@GET
	@Path("/{id:[0-9][0-9]*}")
	@Produces("application/json")
	public EdgeDescriptionDto findById(@PathParam("id") final Long id) {
		EdgeDescription entity = repo().findById(id);
		if (entity == null) {
			throw new NotFoundException();
		}
		return convertTo(entity);
	}

	/**
	 * @param startPosition
	 * @param maxResult
	 * @return
	 */
	@GET
	@Produces("application/json")
	public EdgeDescriptionList listAll(@QueryParam("start") final Integer startPosition, @QueryParam("max") final Integer maxResult) {
		EdgeDescriptionList list = new EdgeDescriptionList();
		repo().findAll().forEach(ed -> list.add(convertTo(ed)));
		return list;
	}

	/**
	 * @param id
	 * @param armordescription
	 * @return
	 */
	@PUT
	@Path("/{id:[0-9][0-9]*}")
	@Consumes("application/json")
	public EdgeDescriptionDto update(@PathParam("id") Long id, final EdgeDescriptionDto dto) {
		if ((id == null) || (id < 0)) {
			throw new IllegalArgumentException("Id must be part of path, and greater than 0.");
		}
		EdgeDescription entity = convertTo(dto);
		entity.setId(id);
		return convertTo(repo().update(entity));
	}

	protected EdgeDescription convertTo(EdgeDescriptionDto dto) {
		EdgeDescription entity = new EdgeDescription();
		entity.setMinimumRank(dto.getMinimumRank());
		if (dto.getEdgeType() != null) {
			entity.setEdgeType(edgeTypeRepo.findById(dto.getEdgeType()));
		}
		entity.setMinimumSkills(dto.getMinimumSkills());
		entity.setName(dto.getName());
		entity.setRequiredEdges(dto.getRequiredEdges());
		entity.setRequiredType(dto.getRequiredType());
		return entity;
	}

	/**
	 * @param id
	 * @return
	 */
	@DELETE
	@Path("/{id:[0-9][0-9]*}")
	public void deleteById(@PathParam("id") final Long id) {

		repo().delete(id);
	}

	protected EdgeDescriptionDto convertTo(EdgeDescription entity) {
		EdgeDescriptionDto dto = new EdgeDescriptionDto();
		dto.setId(entity.getId());
		dto.setVersion(entity.getVersion());
		dto.setEdgeType(entity.getEdgeType().getId());
		dto.setName(entity.getName());
		dto.setMinimumRank(entity.getMinimumRank());
		dto.setRequiredType(entity.getRequiredType());
		dto.setRequiredEdges(entity.getRequiredEdges());
		return dto;
	}

}
