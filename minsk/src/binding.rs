use crate::diagnostic::DiagnosticBag;
use crate::plumbing::Object;
use crate::plumbing::ObjectKind;
use crate::syntax::AssignmentExpressionSyntax;
use crate::syntax::BinaryExpressionSyntax;
use crate::syntax::CompilationUnitSyntaxRef;
use crate::syntax::ExpressionSyntaxRef;
use crate::syntax::UnaryExpressionSyntax;
use crate::text::VariableSymbol;

use self::operators::BoundBinaryOperator;
use self::operators::BoundUnaryOperator;
use self::scope::BoundGlobalScope;
use self::scope::BoundScope;

mod operators;
pub(crate) mod scope;

pub(crate) enum BoundNodeKind {
    BinaryExpression,
    UnaryExpression,
    LiteralExpression,
    VariableExpression,
    AssignmentExpression,
}

pub(crate) enum BoundNode {
    Expression(BoundExpression),
}

impl BoundNode {
    pub(crate) fn kind(&self) -> BoundNodeKind {
        match self {
            BoundNode::Expression(e) => e.kind(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum BoundExpression {
    Binary(BoundBinaryExpression),
    Unary(BoundUnaryExpression),
    Literal(BoundLiteralExpression),
    Variable(BoundVariableExpression),
    Assignment(BoundAssignmentExpression),
}

impl BoundExpression {
    pub(crate) fn kind(&self) -> BoundNodeKind {
        match self {
            BoundExpression::Binary(_) => BoundNodeKind::BinaryExpression,
            BoundExpression::Unary(_) => BoundNodeKind::UnaryExpression,
            BoundExpression::Literal(_) => BoundNodeKind::LiteralExpression,
            BoundExpression::Variable(_) => BoundNodeKind::VariableExpression,
            BoundExpression::Assignment(_) => BoundNodeKind::AssignmentExpression,
        }
    }

    pub(crate) fn get_type(&self) -> ObjectKind {
        match self {
            BoundExpression::Binary(e) => e.operator.result_type,
            BoundExpression::Unary(e) => e.operator.result_type,
            BoundExpression::Literal(e) => e.value.kind(),
            BoundExpression::Variable(e) => e.variable.kind,
            BoundExpression::Assignment(e) => e.expression.get_type(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundBinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LogicalAnd,
    LogicalOr,
    Equality,
    Inequality,
}

#[derive(Debug, Clone)]
pub struct BoundBinaryExpression {
    pub(crate) left: Box<BoundExpression>,
    pub(crate) operator: &'static BoundBinaryOperator,
    pub(crate) right: Box<BoundExpression>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundUnaryOperatorKind {
    Identity,
    Negation,
    LogicalNegation,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundUnaryExpression {
    pub(crate) operator: &'static BoundUnaryOperator,
    pub(crate) operand: Box<BoundExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundLiteralExpression {
    pub(crate) value: Object,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundVariableExpression {
    pub(crate) variable: VariableSymbol,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundAssignmentExpression {
    pub(crate) variable: VariableSymbol,
    pub(crate) expression: Box<BoundExpression>,
}

pub(crate) struct Binder {
    pub(crate) diagnostics: DiagnosticBag,
    scope: BoundScope,
}

impl Binder {
    pub(crate) fn bind_expression(
        &mut self,
        expression: ExpressionSyntaxRef,
    ) -> Box<BoundExpression> {
        match expression {
            ExpressionSyntaxRef::Binary(e) => self.bind_binary_expression(e),
            ExpressionSyntaxRef::Unary(e) => self.bind_unary_expression(e),
            ExpressionSyntaxRef::Literal(e) => self.bind_literal_expression(e),
            ExpressionSyntaxRef::Parenthesized(e) => self.bind_parenthesized_expression(e),
            ExpressionSyntaxRef::Name(e) => self.bind_name_expression(e),
            ExpressionSyntaxRef::Assignment(e) => self.bind_assignment_expression(e),
        }
    }

    fn bind_binary_expression(&mut self, e: &BinaryExpressionSyntax) -> Box<BoundExpression> {
        let left = self.bind_expression(e.left.create_ref());
        let right = self.bind_expression(e.right.create_ref());
        let operator =
            BoundBinaryOperator::bind(e.operator_token.kind, left.get_type(), right.get_type());
        if let Some(operator) = operator {
            Box::new(BoundExpression::Binary(BoundBinaryExpression {
                left,
                operator,
                right,
            }))
        } else {
            self.diagnostics.report_undefined_binary_operator(
                e.operator_token.span(),
                e.operator_token.text.clone(),
                left.get_type(),
                right.get_type(),
            );
            left
        }
    }

    fn bind_unary_expression(&mut self, e: &UnaryExpressionSyntax) -> Box<BoundExpression> {
        let operand = self.bind_expression(e.operand.create_ref());
        let operator = BoundUnaryOperator::bind(e.operator_token.kind, operand.get_type());
        if let Some(operator) = operator {
            Box::new(BoundExpression::Unary(BoundUnaryExpression {
                operator,
                operand,
            }))
        } else {
            self.diagnostics.report_undefined_unary_operator(
                e.operator_token.span(),
                e.operator_token.text.clone(),
                operand.get_type(),
            );
            operand
        }
    }

    fn bind_literal_expression(
        &self,
        e: &crate::syntax::LiteralExpressionSyntax,
    ) -> Box<BoundExpression> {
        Box::new(BoundExpression::Literal(BoundLiteralExpression {
            value: e.value.clone(),
        }))
    }

    fn bind_parenthesized_expression(
        &mut self,
        e: &crate::syntax::ParenthesizedExpressionSyntax,
    ) -> Box<BoundExpression> {
        self.bind_expression(e.expression.create_ref())
    }

    pub(crate) fn new(scope: BoundScope) -> Self {
        Self {
            diagnostics: DiagnosticBag::new(),
            scope,
        }
    }

    fn bind_name_expression(
        &mut self,
        e: &crate::syntax::NameExpressionSyntax,
    ) -> Box<BoundExpression> {
        let name = e.identifier_token.text.clone();

        let variable = self.scope.try_lookup(&name);

        match variable {
            Some(v) => Box::new(BoundExpression::Variable(BoundVariableExpression {
                variable: v.clone(),
            })),
            None => {
                self.diagnostics
                    .report_undefined_name(e.identifier_token.span(), &name);
                Box::new(BoundExpression::Literal(BoundLiteralExpression {
                    value: Object::Number(0),
                }))
            }
        }
    }

    fn bind_assignment_expression(
        &mut self,
        e: &AssignmentExpressionSyntax,
    ) -> Box<BoundExpression> {
        let name = e.identifier_token.text.clone();
        let expression = self.bind_expression(e.expression.create_ref());

        let variable = VariableSymbol {
            name: name.clone(),
            kind: expression.get_type(),
        };

        if !self.scope.try_declare(variable.clone()) {
            self.diagnostics
                .report_variable_already_declared(e.identifier_token.span(), &name);
        }

        Box::new(BoundExpression::Assignment(BoundAssignmentExpression {
            variable,
            expression,
        }))
    }

    pub(crate) fn bind_global_scope(
        previous: Option<&BoundGlobalScope>,
        syntax: CompilationUnitSyntaxRef,
    ) -> BoundGlobalScope {
        let parent_scope = Self::create_parent_scopes(previous);
        let mut binder = Binder::new(parent_scope);
        let expression = binder.bind_expression(syntax.expression);
        let variables = binder
            .scope
            .get_declared_variables()
            .into_iter()
            .cloned()
            .collect();
        let diagnostics = binder.diagnostics.into_iter().collect::<Vec<_>>();
        BoundGlobalScope {
            previous: None,
            diagnostics,
            variables,
            expression: *expression,
        }
    }

    fn create_parent_scopes(mut previous: Option<&BoundGlobalScope>) -> BoundScope {
        let mut stack = Vec::new();
        while let Some(p) = previous {
            stack.push(p);
            previous = p.previous.as_ref().map(|p| p.as_ref());
        }

        let mut parent = BoundScope::new(None);
        while let Some(global) = stack.pop() {
            let mut scope = BoundScope::new(Some(Box::new(parent)));
            for v in &global.variables {
                scope.try_declare(v.clone());
            }
            parent = scope;
        }
        parent
    }
}
