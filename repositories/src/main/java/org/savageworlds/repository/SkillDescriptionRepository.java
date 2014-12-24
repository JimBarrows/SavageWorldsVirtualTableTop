package org.savageworlds.repository;

import java.util.List;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
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

	public SkillDescription findById(Long id) {		
		return em.find( SkillDescription.class, id);
	}

	@SuppressWarnings("unchecked")
	public List<SkillDescription> all() {
		Query query = em.createQuery("SELECT e FROM SkillDescription e");
	    return (List<SkillDescription>) query.getResultList();
	}
}
