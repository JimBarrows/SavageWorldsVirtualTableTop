package org.savageworlds.repository;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.validation.Valid;

import org.savageworlds.game.model.SkillDescription;

@Stateless
public class SkillDescriptionRepository {

	@PersistenceContext(name="SavageWorlds")
	private EntityManager em ;
	
	public SkillDescription create(@Valid SkillDescription sd) {
		em.persist(sd);
		return sd;
	}
}
