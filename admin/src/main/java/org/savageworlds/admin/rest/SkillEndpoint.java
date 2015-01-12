package org.savageworlds.admin.rest;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.transaction.Transactional;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

import org.savageworlds.admin.dto.DiceTypeDto;
import org.savageworlds.admin.dto.SkillDto;
import org.savageworlds.game.model.DiceType;
import org.savageworlds.game.model.EdgeDescription;
import org.savageworlds.game.model.Skill;
import org.savageworlds.game.model.SkillDescription;
import org.savageworlds.repository.EdgeDescriptionRepository;
import org.savageworlds.repository.SkillDescriptionRepository;

@RequestScoped
@Path("/skills")
public class SkillEndpoint  {

	@EJB
	private EdgeDescriptionRepository edgeDescriptionRepo;
	
	@EJB
	private SkillDescriptionRepository skillDescriptionRepo;
	
	@PersistenceContext(name = "SavageWorlds", type = PersistenceContextType.EXTENDED)
	private EntityManager	em;
	
	/**
	 * @param armordescription
	 * @return
	 */
	@POST
	@Consumes("application/json")
	public SkillDto create( SkillDto dto) { 
		EdgeDescription edgeDescription = edgeDescriptionRepo.findById(dto.getEdge());
		SkillDescription skillDescription = skillDescriptionRepo.findById(dto.getDescription());
		DiceType diceType = DiceType.valueOf(dto.getDice());
		Skill skill = edgeDescription.addSkill( skillDescription, diceType);
		em.persist(skill);
		edgeDescriptionRepo.update(edgeDescription);
		dto.setId( skill.getId());
		dto.setVersion( skill.getVersion());
		return dto;
	}
		
	@GET
	@Path("/{id}")
	@Produces("application/json")
	public SkillDto findById(@PathParam("id") final Long id) {		
		Skill skill = em.find(Skill.class, id);
		SkillDto dto = new SkillDto();
		dto.setId(skill.getId());
		dto.setVersion(skill.getVersion());
		dto.setBonus(skill.getBonus());
		dto.setDescription(skill.getSkill().getId());
		dto.setDice(skill.getDice().name());
		
		return dto;
	}
	
	
}
