use std::collections::HashMap;

use crate::diagnostic::DiagnosticBag;
use crate::plumbing::Object;
use crate::plumbing::ObjectKind;
use crate::syntax::BinaryExpressionSyntax;
use crate::syntax::ExpressionSyntaxRef;
use crate::syntax::UnaryExpressionSyntax;
use crate::text::VariableSymbol;

use self::operators::BoundBinaryOperator;
use self::operators::BoundUnaryOperator;

mod operators;

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

pub(crate) struct BoundUnaryExpression {
    pub(crate) operator: &'static BoundUnaryOperator,
    pub(crate) operand: Box<BoundExpression>,
}

pub(crate) struct BoundLiteralExpression {
    pub(crate) value: Object,
}

pub(crate) struct BoundVariableExpression {
    pub(crate) variable: VariableSymbol,
}

pub(crate) struct BoundAssignmentExpression {
    pub(crate) variable: VariableSymbol,
    pub(crate) expression: Box<BoundExpression>,
}

pub(crate) struct Binder<'v> {
    pub(crate) diagnostics: DiagnosticBag,
    variables: &'v mut HashMap<VariableSymbol, Object>,
}

impl<'v> Binder<'v> {
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

    pub(crate) fn new(variables: &'v mut HashMap<VariableSymbol, Object>) -> Self {
        Self {
            diagnostics: DiagnosticBag::new(),
            variables,
        }
    }

    fn bind_name_expression(
        &mut self,
        e: &crate::syntax::NameExpressionSyntax,
    ) -> Box<BoundExpression> {
        let name = e.identifier_token.text.clone();

        let variable = self.variables.keys().find(|k| k.name == name);

        match variable {
            Some(v) => Box::new(BoundExpression::Variable(BoundVariableExpression {
                variable: v.clone(),
            })),
            None => {
                self.diagnostics
                    .report_undefined_name(e.identifier_token.span(), name);
                Box::new(BoundExpression::Literal(BoundLiteralExpression {
                    value: Object::Number(0),
                }))
            }
        }
    }

    fn bind_assignment_expression(
        &mut self,
        e: &crate::syntax::AssignmentExpressionSyntax,
    ) -> Box<BoundExpression> {
        let name = e.identifier_token.text.clone();
        let expression = self.bind_expression(e.expression.create_ref());

        let existing_variable = self.variables.keys().find(|k| k.name == name);
        if let Some(existing_variable) = existing_variable.cloned() {
            self.variables.remove(&existing_variable);
        }
        let variable = VariableSymbol {
            name,
            kind: expression.get_type(),
        };
        let default_value = match expression.get_type() {
            ObjectKind::Number => Object::Number(0),
            ObjectKind::Boolean => Object::Boolean(false),
            ObjectKind::Null => Object::Null,
        };

        if default_value == Object::Null {
            panic!("Unsupported variable type {:?}", expression.get_type());
        }

        self.variables.insert(variable.clone(), default_value);

        Box::new(BoundExpression::Assignment(BoundAssignmentExpression {
            variable,
            expression,
        }))
    }
}
