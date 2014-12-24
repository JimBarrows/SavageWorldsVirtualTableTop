package org.savageworlds.repository;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;

import jdo.core.repository.RepositoryTemplate;

import org.savageworlds.game.model.SkillDescription;

@Stateless
public class SkillDescriptionRepository extends RepositoryTemplate<SkillDescription, Long>{

	@PersistenceContext(name = "SavageWorlds", type = PersistenceContextType.EXTENDED)
	protected EntityManager em;
	
	public SkillDescriptionRepository() {
		super(SkillDescription.class);
	}

	@Override
	protected EntityManager em() {
		return em;
	}
	
	
	
}
