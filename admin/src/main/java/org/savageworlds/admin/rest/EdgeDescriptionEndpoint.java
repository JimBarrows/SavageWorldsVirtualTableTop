package org.savageworlds.admin.rest;

import java.util.ArrayList;
import java.util.List;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.transaction.Transactional;
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
import org.savageworlds.game.model.Skill;
import org.savageworlds.game.model.SkillDescription;
import org.savageworlds.repository.EdgeDescriptionRepository;
import org.savageworlds.repository.EdgeTypeRepository;

@RequestScoped
@Path("/edgeDescriptions")
public class EdgeDescriptionEndpoint {

	@EJB
	private EdgeDescriptionRepository repo;

	@EJB
	private EdgeTypeRepository edgeTypeRepo;

	@PersistenceContext(name = "SavageWorlds", type = PersistenceContextType.EXTENDED)
	private EntityManager em;

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
	public EdgeDescriptionList listAll(
			@QueryParam("start") final Integer startPosition,
			@QueryParam("max") final Integer maxResult) {
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
	@Transactional
	public EdgeDescriptionDto update(@PathParam("id") Long id,
			final EdgeDescriptionDto dto) {
		if ((id == null) || (id < 0)) {
			throw new IllegalArgumentException(
					"Id must be part of path, and greater than 0.");
		}
		EdgeDescription newEntity = convertTo(dto);
		EdgeDescription originalEntity = em.find(EdgeDescription.class, id);
		originalEntity.setEdgeType(newEntity.getEdgeType());
		originalEntity.setMinimumRank(newEntity.getMinimumRank());
		originalEntity.setMinimumSkills(newEntity.getMinimumSkills());
		originalEntity.setName(newEntity.getName());
		originalEntity.setRequiredEdges(newEntity.getRequiredEdges());
		originalEntity = em.merge(originalEntity);
		return convertTo(originalEntity);
	}

	protected EdgeDescription convertTo(EdgeDescriptionDto dto) {
		EdgeDescription entity =  (dto.getId() == null) ? new EdgeDescription() : repo.findById(dto.getId());
		
		entity.setMinimumRank(dto.getMinimumRank());
		if (dto.getEdgeType() != null) {
			entity.setEdgeType(edgeTypeRepo.findById(dto.getEdgeType()));
		}
		if (dto.getMinimumSkills() == null || dto.getMinimumSkills().isEmpty()) {
			entity.setMinimumSkills(new ArrayList<Skill>());
		} else {
			entity.setMinimumSkills(new ArrayList<Skill>());
			dto.getMinimumSkills().forEach(
					skillId -> entity.addSkill(em.find(Skill.class, skillId)));
		}
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
		dto.setName(entity.getName());
		dto.setEdgeType(entity.getEdgeType().getId());
		dto.setMinimumRank(entity.getMinimumRank());
		dto.setRequiredType(entity.getRequiredType());
		if ((entity.getMinimumSkills() == null)
				|| (entity.getMinimumSkills().isEmpty())) {
			dto.setMinimumSkills(null);
		} else {
			List<Long> skillIdList = new ArrayList<Long>();
			entity.getMinimumSkills().forEach(
					skill -> skillIdList.add(skill.getId()));
			dto.setMinimumSkills(skillIdList);
		}
		if ((entity.getRequiredEdges() == null)
				|| (entity.getRequiredEdges().isEmpty())) {
			dto.setRequiredEdges(null);
		} else {
			dto.setRequiredEdges(entity.getRequiredEdges());
		}
		return dto;
	}
}
